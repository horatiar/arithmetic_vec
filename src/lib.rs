#![feature(nonnull_slice_from_raw_parts)]

use core::{
	cmp::{
		Ordering, Ordering::*,
	},
	fmt::{
		Debug,
	},
	hash::{
		Hash, Hasher
	},
	marker::{
		PhantomData,
	},
	ops::{
		Index, IndexMut,
	},
	ptr::{
		NonNull,
	},
};

pub use self::ArithmeticLevel::{
	Raw, 
	Virt,
};

#[cfg(feature="serde")]
use serde::{Deserialize, Serialize};

#[derive(Copy,Clone,Debug,Eq,Hash,Ord,PartialOrd,PartialEq)] #[cfg_attr(feature="serde", derive(Serialize,Deserialize))]
pub enum ArithmeticLevel {
	Raw(usize),
	Virt(usize),
}

impl From<usize> for ArithmeticLevel {
	fn from(raw_level: usize) -> Self {
		Raw(raw_level)
	}
}

/// Calculate the index of the starting level of a
/// complete arithmetic tree; i.e., a complete tree
/// modeled on an arithmetic series.
/// 
/// A series that starts with 1 element then
/// adds 1 more to each level allows a fully
/// connected 2-lattice to be stored within an
/// array without the need for parent-child
/// pointers or fragmented memory.
/// 
/// The parameter `skip` allows only selected
/// levels to be physically stored. The
/// intended use here is to construct a binomial
/// probability tree but only store levels at
/// specific intervals.
/// 
/// ```
/// # use arithmetic_vec::*;
/// assert_eq!(0, arithmetic_idx(Raw(0),0,0));
/// assert_eq!(1, arithmetic_idx(Raw(1),0,0));
/// assert_eq!(3, arithmetic_idx(Raw(2),0,0));
/// assert_eq!(6, arithmetic_idx(Raw(3),0,0));
/// assert_eq!(15, arithmetic_idx(Raw(5),0,0));
/// 
/// assert_eq!(0, arithmetic_idx(Raw(0),0,1));
/// assert_eq!(1, arithmetic_idx(Raw(1),0,1));
/// assert_eq!(4, arithmetic_idx(Raw(2),0,1));
/// assert_eq!(9, arithmetic_idx(Raw(3),0,1));
/// assert_eq!(25, arithmetic_idx(Raw(5),0,1));
/// 
/// assert_eq!(5, arithmetic_idx(Raw(2),0,2));
/// assert_eq!(12, arithmetic_idx(Raw(3),0,2));
/// 
/// assert_eq!(0, arithmetic_idx(Virt(0),0,2));
/// assert_eq!(arithmetic_idx(Raw(4),0,2), arithmetic_idx(Virt(12),0,2));
/// ```
#[inline(always)]
pub fn arithmetic_idx(level: ArithmeticLevel, offset: usize, skip: usize) -> usize {
	let (level, effective_level) = match level {
		Raw(level) => (level, (skip+1)*level),
		Virt(virt) => (virt/(skip+1), virt)
	};
	debug_assert!(effective_level % (skip+1) == 0);

	if level == 0 {
		return 0;
	}
	1 + ((level-1)*(2+effective_level))/2 + offset
}

#[derive(Clone)] #[cfg_attr(feature="serde", derive(Serialize,Deserialize))]
pub struct ArithmeticVec<'a, V, F>
where
	V: 'a,
	F: Fn() -> V
{
	vec: Vec<V>,
	levels: usize,
	maker: F,
	_phantom: PhantomData<&'a mut Vec<V>>,
}

impl<'a, V, F> ArithmeticVec<'a, V, F>
where
	F: Fn() -> V
{
	/// ```
	/// # use arithmetic_vec::*;
	/// assert!(ArithmeticVec::new(&i32::default).level_count() == 0); 
	/// ```
	pub fn new(maker: F) -> Self {
		Self::with_capacity(10, maker)
	}
	/// ```
	/// # use arithmetic_vec::*;
	/// let mut av = ArithmeticVec::with_capacity(0, || 42i32);
	/// assert!(av.level_count() == 0);
	/// av[3][0] = 12;
	/// assert!(av.level_count() == 4);
	/// assert!(av[(0,0)] == 42);
	/// assert!(av[(2,2)] == 42);
	/// assert!(av[(3,3)] == 42);
	/// ```
	pub fn with_capacity(cap: usize, maker: F) -> Self {
		let capacity = arithmetic_idx(Raw(cap),0,0);
		Self {
			vec: Vec::with_capacity(capacity),
			levels: 0,
			maker,
			_phantom: PhantomData
		}
	}

	#[inline]
	pub fn iter_mut(&mut self) -> impl Iterator<Item=&mut[V]> {
		AVecMutIter::new(self)
	}
	
	#[inline]
	pub fn iter(&self) -> impl Iterator<Item=&'a [V]>  {
		AVecIter::new(self)
	}
	
	#[inline]
	pub fn levels(&self) -> impl Iterator<Item=&[V]> {
		self.iter()
	}
	
	pub fn level_count(&self) -> usize {
		self.levels
	}
	
	#[inline(always)]
	#[allow(clippy::same_item_push)]
	pub fn reserve(&mut self, levels: usize) {
		if self.levels <= levels {
			let cur_items = self.vec.len();
			let req_items = arithmetic_idx(Raw(levels+1),0,0);
			let len = req_items - cur_items;
			self.vec.reserve(len);
			for _ in 0..len {
				self.vec.push((self.maker)()); // clippy::same_item_push is wrong
			}
			debug_assert!(self.vec.len() == cur_items + len);
			debug_assert!(self.vec.len() == req_items);
			self.levels = levels+1;
		}
	}
}

impl<'a, V, F> Debug for ArithmeticVec<'a, V, F>
where
	F: Fn() -> V
{
	fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
		todo!()
	}
}

impl<'a, V, F> Index<usize> for ArithmeticVec<'a, V, F>
where
	F: Fn() -> V
{
	type Output = [V];
	
	/// ```
	/// # use arithmetic_vec::*;
	/// let mut av = ArithmeticVec::new(||0);
	/// av[(2,1)] = 42;
	/// assert!(av[(2,1)] == 42);
	/// assert!(av[2] == [0,42,0]);
	/// ```
	#[inline]
	fn index(&self, level: usize) -> &[V] {
		let range = arithmetic_idx(Raw(level),0,0) .. arithmetic_idx(Raw(level+1),0,0);
		&self.vec[range]
	}
}

impl<'a, V, F> IndexMut<usize> for ArithmeticVec<'a, V, F>
where
	F: Fn() -> V
{
	/// ```
	/// # use arithmetic_vec::*;
	/// let mut av = ArithmeticVec::new(||0);
	/// av[2][1] = 42;
	/// assert!(av[(2,1)] == 42);
	/// assert!(av[2] == [0,42,0]);
	/// let mut _av_lvl_3 : &mut[i32] = &mut av[2];
	/// ```
	#[inline]
	fn index_mut(&mut self, level: usize) -> &mut [V] {
		self.reserve(level);
		let range = arithmetic_idx(Raw(level),0,0) .. arithmetic_idx(Raw(level+1),0,0);
		&mut self.vec[range]
	}
}

impl<'a, V, F> Index<(usize,usize)> for ArithmeticVec<'a, V, F>
where
	F: Fn() -> V
{
	type Output = V;
	#[inline]
	fn index(&self, (level,idx): (usize,usize)) -> &V {
		&self.vec[arithmetic_idx(Raw(level),idx,0)]
	}
}

impl<'a, V, F> IndexMut<(usize,usize)> for ArithmeticVec<'a, V, F>
where
	F: Fn() -> V
{
	#[inline]
	fn index_mut(&mut self, (level,idx): (usize,usize)) -> &mut V {
		self.reserve(level);
		&mut self.vec[arithmetic_idx(Raw(level),idx,0)]
	}
}

impl<'a, V, F> AsRef<ArithmeticVec<'a, V, F>> for ArithmeticVec<'a, V, F>
where
	F: Fn() -> V
{
	fn as_ref(&self) -> &ArithmeticVec<'a, V, F> {
		self
	}
}

impl<'a, V, F> AsMut<ArithmeticVec<'a, V, F>> for ArithmeticVec<'a, V, F>
where
	F: Fn() -> V
{
	fn as_mut(&mut self) -> &mut ArithmeticVec<'a, V, F> {
		self
	}
}

impl<'a, 'b, A, B, F, G> PartialEq<ArithmeticVec<'b, B, G>> for ArithmeticVec<'a, A, F>
where
	A: 'a + PartialEq<B>,
	B: 'b,
	F: Fn() -> A,
	G: Fn() -> B
{
	fn eq(&self, rhs: &ArithmeticVec<B, G>) -> bool {
		self.vec.eq(&rhs.vec)
	}
}

impl<'a, V: 'a+Eq, F: Fn() -> V> Eq for ArithmeticVec<'a, V, F> {}

impl<'a, V, F> PartialOrd<ArithmeticVec<'a, V, F>> for ArithmeticVec<'a, V, F>
where
	V: 'a + PartialOrd<V>,
	F: Fn() -> V
{
	fn partial_cmp(&self, rhs: &ArithmeticVec<V, F>) -> Option<Ordering> {
		match self.vec.len().cmp(&rhs.vec.len()) {
			Equal => self.vec.partial_cmp(&rhs.vec),
			Less => match &self.vec[..].partial_cmp(&rhs.vec[0..self.vec.len()]) {
				Some(Equal) => Some(Less),
				Some(ord) => Some(*ord),
				None => None
			},
			Greater => match self.vec[0..rhs.vec.len()].partial_cmp(&rhs.vec) {
				Some(Equal) => Some(Greater),
				Some(ord) => Some(ord),
				None => None
			},
		}
	}
}

impl<'a, V, F> Ord for ArithmeticVec<'a, V, F>
where
	V: 'a + Ord,
	F: Fn() -> V
{
	fn cmp(&self, rhs: &ArithmeticVec<V, F>) -> Ordering {
		match self.vec.len().cmp(&rhs.vec.len()) {
			Equal => self.vec.cmp(&rhs.vec),
			Less => match &self.vec[..].cmp(&rhs.vec[0..self.vec.len()]) {
				Equal => Less,
				ord => *ord,
			},
			Greater => match self.vec[0..rhs.vec.len()].cmp(&rhs.vec) {
				Equal => Greater,
				ord => ord,
			},
		}
	}
}

impl<'a, V, F> Hash for ArithmeticVec<'a, V, F>
where
	V: 'a + Hash,
	F: Fn() -> V
{
	fn hash<H: Hasher>(&self, state: &mut H) {
		Hash::hash(&self.vec, state);
		Hash::hash(&self.levels, state);
	}
}

pub struct AVecIter<'a, V: 'a> {
	cur_ptr: NonNull<V>,
	cur: usize,
	len: usize,
	_phantom: PhantomData<&'a V>,
}

impl<'a, V: 'a> AVecIter<'a, V> {
	pub fn new<F: Fn() -> V>(source: &ArithmeticVec<'a, V, F>) -> Self {
		AVecIter {
			cur_ptr: NonNull::new(source.vec.as_ptr() as *mut V).unwrap(),
			cur: 0,
			len: source.levels,
			_phantom: PhantomData
		}
	}
}

impl<'a, V: 'a> Iterator for AVecIter<'a, V> {
	type Item = &'a[V];
	
	#[inline]
	fn next(&mut self) -> Option<Self::Item> {
		debug_assert!(self.cur <= self.len);
		if self.cur == self.len {
			return None
		}
		self.cur += 1; // pre-increment because each level has level+1 elements
		let slice = NonNull::slice_from_raw_parts(self.cur_ptr, self.cur);
		unsafe {
			self.cur_ptr = NonNull::new_unchecked(self.cur_ptr.as_ptr().add(self.cur));
			Some(&*slice.as_ptr())
		}
	}
	
	#[inline]
	fn size_hint(&self) -> (usize, Option<usize>) {
		(self.len, Some(self.len))
	}
	
	#[inline]
	fn count(self) -> usize {
		self.len
	}
}

#[cfg(feature="exact_size_is_empty")]
impl<'a, V: 'a> ExactSizeIterator for AVecIter<'a, V> {
	#[inline]
	fn is_empty(&self) -> bool {
		self.len == 0
	}
}

pub struct AVecMutIter<'a, V: 'a> {
	cur_ptr: NonNull<V>,
	cur: usize,
	len: usize,
	_phantom: PhantomData<&'a V>,
}

impl<'a, V: 'a> AVecMutIter<'a, V> {
	pub fn new<F: Fn() -> V>(source: &mut ArithmeticVec<'a, V, F>) -> Self {
		AVecMutIter {
			cur_ptr: NonNull::new(source.vec.as_mut_ptr()).unwrap(),
			cur: 0,
			len: source.levels,
			_phantom: PhantomData
		}
	}
}

impl<'a, V: 'a> Iterator for AVecMutIter<'a, V> {
	type Item = &'a mut[V];
	
	#[inline]
	fn next(&mut self) -> Option<Self::Item> {
		debug_assert!(self.cur <= self.len);
		if self.cur == self.len {
			return None
		}
		self.cur += 1; // pre-increment because each level has level+1 elements
		let slice = NonNull::slice_from_raw_parts(self.cur_ptr, self.cur);
		unsafe {
			self.cur_ptr = NonNull::new_unchecked(self.cur_ptr.as_ptr().add(self.cur));
			Some(&mut *slice.as_ptr())
		}
	}
	
	#[inline]
	fn size_hint(&self) -> (usize, Option<usize>) {
		(self.len, Some(self.len))
	}
	
	#[inline]
	fn count(self) -> usize {
		self.len
	}
}

#[cfg(feature="exact_size_is_empty")]
impl<'a, V: 'a> ExactSizeIterator for AVecMutIter<'a, V> {
	#[inline]
	fn is_empty(&self) -> bool {
		self.len == 0
	}
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
