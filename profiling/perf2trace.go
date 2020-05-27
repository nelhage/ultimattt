package main

import (
	"bufio"
	"encoding/json"
	"fmt"
	"log"
	"os"
	"regexp"
	"strconv"
	"strings"
)

type TraceEvent struct {
	Name string      `json:"name"`
	Cat  string      `json:"cat,omitempty"`
	Ph   string      `json:"ph"`
	Ts   uint64      `json:"ts"`
	Dur  uint64      `json:"dur,omitempty"`
	TDur uint64      `json:"tdur,omitempty"`
	Pid  uint64      `json:"pid"`
	Tid  uint64      `json:"tid"`
	Sf   uint64      `json:"sf,omitempty"`
	Args interface{} `json:"args,omitempty"`
}

type StackFrame struct {
	Name     string `json:"name"`
	Parent   string `json:"parent,omitempty"`
	Category string `json:"category,omitempty"`
}

type Trace struct {
	TraceEvents []TraceEvent          `json:"traceEvents"`
	StackFrames map[uint64]StackFrame `json:"stackFrames"`
}

type rawFrame struct {
	addr   uint64
	symbol string
	dso    string
}

type Frame struct {
	Id       uint64
	Parent   *Frame
	Name     string
	Children map[string]*Frame
}

type frameTree struct {
	root   *Frame
	nextId uint64
}

func (ft *frameTree) newFrame(parent *Frame, name string) *Frame {
	f := &Frame{
		Id:       ft.nextId,
		Name:     name,
		Parent:   parent,
		Children: make(map[string]*Frame),
	}
	ft.nextId += 1
	return f
}

func (ft *frameTree) insert(stack []rawFrame) *Frame {
	if ft.root == nil {
		ft.nextId = 0
		ft.root = ft.newFrame(nil, "<root>")
	}
	cursor := ft.root
	for i := len(stack) - 1; i >= 0; i-- {
		frame := stack[i]
		if next, ok := cursor.Children[frame.symbol]; ok {
			cursor = next
		} else {
			f := ft.newFrame(cursor, frame.symbol)
			cursor.Children[frame.symbol] = f
			cursor = f
		}
	}
	return cursor
}

func path(f *Frame, buf []*Frame) []*Frame {
	for f != nil {
		buf = append(buf, f)
		f = f.Parent
	}
	return buf
}

func (ft *frameTree) lca(left *Frame, right *Frame) *Frame {
	var lbuf [30]*Frame
	var rbuf [30]*Frame
	lpath := path(left, lbuf[:0])
	rpath := path(right, rbuf[:0])
	l := len(lpath) - 1
	r := len(rpath) - 1
	if lpath[l] != ft.root {
		panic("left is not part of this tree")
	}
	if rpath[r] != ft.root {
		panic("right is not part of this tree")
	}
	for l >= 0 && r >= 0 && lpath[l] == rpath[r] {
		l -= 1
		r -= 1
	}
	return lpath[l+1]
}

func (ft *frameTree) walk(cb func(*Frame)) {
	ft.rwalk(ft.root, cb)
}
func (ft *frameTree) rwalk(f *Frame, cb func(*Frame)) {
	cb(f)
	for _, ch := range f.Children {
		ft.rwalk(ch, cb)
	}
}

type Event struct {
	Event    string
	Comm     string
	Pid, Tid uint64
	Time     uint64
	Frame    *Frame
}

func mustUint(text []byte) uint64 {
	if n, e := strconv.ParseUint(string(text), 10, 64); e != nil {
		panic(fmt.Sprintf("parse uint: %v", e))
	} else {
		return n
	}
}

func mustUintHex(text []byte) uint64 {
	if n, e := strconv.ParseUint(string(text), 16, 64); e != nil {
		panic(fmt.Sprintf("parse hex uint: %v", e))
	} else {
		return n
	}
}

func main() {
	// Run as: `perf script -F comm,pid,tid,time,event,ip,sym,dso | perf2trace`
	pat := struct {
		header *regexp.Regexp
		frame  *regexp.Regexp
	}{
		header: regexp.MustCompile(`^(\S+)\s+(\d+)/(\d+)\s*(\d+)[.](\d+):\s*(\S+):`),
		frame:  regexp.MustCompile(`^\s+([0-9a-f]+)\s+(\S+)\s+[(](\S+)[)]$`),
	}

	scan := bufio.NewScanner(os.Stdin)
	var stack []rawFrame
	var trace []Event
	var current Event
	var tree frameTree
	for scan.Scan() {
		line := scan.Bytes()
		if m := pat.header.FindSubmatch(line); m != nil {
			var (
				comm  = string(m[1])
				pid   = mustUint(m[2])
				tid   = mustUint(m[3])
				t_s   = mustUint(m[4])
				t_us  = mustUint(m[5])
				event = string(m[6])
			)
			if len(stack) > 0 {
				current.Frame = tree.insert(stack)
				trace = append(trace, current)
			}
			current = Event{
				Comm:  comm,
				Pid:   pid,
				Tid:   tid,
				Event: event,
				Time:  t_s*1000000 + t_us,
			}
			stack = stack[:0]
		} else if m := pat.frame.FindSubmatch(line); m != nil {
			var (
				addr  = mustUintHex(m[1])
				frame = string(m[2])
				dso   = string(m[3])
			)
			if dso == "inlined" || strings.HasSuffix(dso, "ultimattt") {
				stack = append(stack, rawFrame{addr, frame, dso})
			}
		}
	}
	if err := scan.Err(); err != nil {
		log.Fatal("scan error: %v", err)
	}

	out := marshal(&tree, trace)

	encode := json.NewEncoder(os.Stdout)
	encode.Encode(&out)
}

func marshal(tree *frameTree, trace []Event) Trace {
	out := Trace{
		StackFrames: make(map[uint64]StackFrame),
	}
	type thread struct {
		tid, pid uint64
		frame    *Frame
		stack    []uint64
		ts       uint64
	}
	stack := make(map[uint64]*thread)
	var buf [64]TraceEvent
	for _, event := range trace {
		prev, ok := stack[event.Tid]
		if !ok {
			prev = &thread{frame: tree.root, tid: event.Tid, pid: event.Pid}
			stack[event.Tid] = prev
		}
		lca := tree.lca(prev.frame, event.Frame)

		/*
			fmt.Fprintf(os.Stderr, "tid=%d lca=%s\n", event.Tid, lca.Name)
			fmt.Fprintf(os.Stderr, " left stack=")
			for f := prev.frame; f != nil; f = f.Parent {
				fmt.Fprintf(os.Stderr, " %s <-", f.Name)
			}
			fmt.Fprintf(os.Stderr, "\n rigt stack=")
			for f := event.Frame; f != nil; f = f.Parent {
				fmt.Fprintf(os.Stderr, " %s <-", f.Name)
			}
			fmt.Fprintln(os.Stderr)
		*/

		tmp := buf[:0]
		for f := prev.frame; f != lca; f = f.Parent {
			entry := prev.stack[len(prev.stack)-1]
			prev.stack = prev.stack[:len(prev.stack)-1]

			te := TraceEvent{
				Name: f.Name,
				Ph:   "X",
				Ts:   entry,
				Dur:  event.Time - entry,
				Sf:   f.Id,
				Pid:  event.Pid,
				Tid:  event.Tid,
			}
			tmp = append(tmp, te)
		}
		for i := 0; i < len(tmp)/2; i++ {
			tmp[i], tmp[len(tmp)-1-i] = tmp[len(tmp)-1-i], tmp[i]
		}
		out.TraceEvents = append(out.TraceEvents, tmp...)

		for f := event.Frame; f != lca; f = f.Parent {
			prev.stack = append(prev.stack, event.Time)
		}

		prev.frame = event.Frame
		prev.ts = event.Time
	}

	tmp := buf[:0]
	for tid, thr := range stack {
		for f := thr.frame; f != tree.root; f = f.Parent {
			entry := thr.stack[len(thr.stack)-1]
			thr.stack = thr.stack[:len(thr.stack)-1]
			te := TraceEvent{
				Name: f.Name,
				Ph:   "X",
				Ts:   entry,
				Dur:  thr.ts - entry,
				Sf:   f.Id,
				Pid:  thr.pid,
				Tid:  tid,
			}
			tmp = append(tmp, te)
		}
	}
	for i := 0; i < len(tmp)/2; i++ {
		tmp[i], tmp[len(tmp)-1-i] = tmp[len(tmp)-1-i], tmp[i]
	}
	out.TraceEvents = append(out.TraceEvents, tmp...)

	tree.walk(func(f *Frame) {
		if f.Id == 0 {
			return
		}
		out.StackFrames[f.Id] = StackFrame{
			Name:   f.Name,
			Parent: strconv.FormatUint(f.Parent.Id, 10),
		}
	})

	return out
}
