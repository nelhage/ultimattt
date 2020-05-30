use bytesize::ByteSize;
use hdrhistogram::Histogram;
use serde;
use serde::ser::SerializeStruct;

use std::path::Path;

pub fn serialize_histogram<S: serde::Serializer, T: hdrhistogram::Counter>(
    hist: &Histogram<T>,
    serializer: S,
) -> Result<S::Ok, S::Error> {
    let mut state = serializer.serialize_struct("histogram", 5)?;
    {
        let f = hist.mean();
        state.serialize_field("mean", &f)?;
    }
    {
        let f = hist.value_at_quantile(0.1);
        state.serialize_field("p10", &f)?;
    }
    {
        let f = hist.value_at_quantile(0.5);
        state.serialize_field("p50", &f)?;
    }
    {
        let f = hist.value_at_quantile(0.9);
        state.serialize_field("p90", &f)?;
    }
    {
        let f = hist.value_at_quantile(0.99);
        state.serialize_field("p99", &f)?;
    }
    state.end()
}

pub fn read_rss() -> ByteSize {
    let path = Path::new("/proc/")
        .join(std::process::id().to_string())
        .join("stat");
    let stat = std::fs::read_to_string(path).unwrap();
    let mut bits = stat.split(' ');
    ByteSize::kib(4 * bits.nth(23).unwrap().parse::<u64>().unwrap())
}
