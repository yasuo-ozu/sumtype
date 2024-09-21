use sumtype::{sumtrait, sumtype};

struct Marker;

#[sumtrait(marker = Marker)]
trait MySumTrait {
    fn f(&self) -> usize;
}
