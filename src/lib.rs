use core::{
	cmp::{Ordering, Ordering::*},
	fmt::{Debug},
	hash::{Hash, Hasher},
	marker::{PhantomData},
	ops::{Index, IndexMut},
	ptr::{NonNull},
    slice,
};

#[cfg(feature="serde")]
use serde::{Deserialize, Serialize};

/// Calculate the index of the starting level of a
/// complete arithmetic tree; i.e., a complete tree
/// modeled on an arithmetic series.
/// 
/// A series that starts with 1 element then
/// adds 1 more to each level allows a fully
/// connected 2-lattice to be stored within an
/// array without the need for parent-child
/// pointers or fragmented memory.
/// ```
/// # use arithmetic_vec::*;
/// assert_eq!(0, arithmetic_idx(0,0));
/// assert_eq!(1, arithmetic_idx(1,0));
/// assert_eq!(3, arithmetic_idx(2,0));
/// assert_eq!(6, arithmetic_idx(3,0));
/// assert_eq!(15, arithmetic_idx(5,0));
/// ```
#[inline(always)]
pub fn arithmetic_idx(level: usize, offset: usize) -> usize {
	if level == 0 {
		return 0; // 0 - 1 below violates usize
	}

	1 + ((level-1)*(2+level))/2 + offset
}

pub fn level_of(offset: usize, total_levels: usize) -> Result<(usize,usize),String> {
    let mut min = 0;
    let mut max = total_levels;
    while min <= max {
        let mid = (min+max)/2;
        let mid_idx = arithmetic_idx(mid, 0);

        if max - min <= 1 {
            debug_assert!(min <= max);
            let min_idx = arithmetic_idx(min, 0);
            return Ok((min,offset-min_idx))
        } else if offset < mid_idx {
            max = mid;
        } else if offset > mid_idx {
            min = mid;
        } else /* offset == mid_idx */ {
            min = mid
        }
    }
    Err("total_levels not big enough".to_string())
}

#[derive(Clone)] #[cfg_attr(feature="serde", derive(Serialize,Deserialize))]
pub struct ArithmeticVec<'a, V:'a+Default> {
	vec: Vec<V>,
	levels: usize,
	_phantom: PhantomData<&'a mut Vec<V>>,
}

impl<'a, V:'a+Default> ArithmeticVec<'a, V> {
	/// ```
	/// # use arithmetic_vec::*;
	/// let mut av = ArithmeticVec::with_capacity(0);
	/// assert!(av.level_count() == 0);
	/// av[3][0] = 12;
	/// assert!(av.level_count() == 4);
	/// assert!(av[(0,0)] == 0);
	/// assert!(av[(2,2)] == 0);
	/// assert!(av[(3,3)] == 0);
	/// ```
	pub fn with_capacity(cap: usize) -> Self {
		let capacity = arithmetic_idx(cap,0);
		Self {
			vec: Vec::with_capacity(capacity),
			levels: 0,
			_phantom: PhantomData
		}
	}

	#[inline(always)]
	pub fn enumerate_mut(&mut self) -> impl Iterator<Item=(usize, &mut[V])> {
		AVecMutIter::new(self).enumerate()
	}
	
	#[inline(always)]
	pub fn enumerate(&self) -> impl Iterator<Item=(usize, &'a [V])>  {
		AVecIter::new(self).enumerate()
	}

	#[inline(always)]
    pub fn enumerate_items_mut(&'a mut self) -> impl Iterator<Item=(usize, usize, &mut V)> {
        let level_count = self.level_count();
        self.vec.iter_mut().enumerate().map(move |(v_idx,item)|
            match level_of(v_idx, level_count) {
                Ok((level, offset)) => (level, offset, item),
                Err(_) => panic!("logic error")
            }
        )
    }
	
	/// ```
	/// # use arithmetic_vec::*;
	/// let mut av = ArithmeticVec::default();
	/// av[(2,1)] = 42i32;
	/// assert_eq!(av.enumerate_items().map(|(level,offset,ref_item)|(level,offset,*ref_item)).collect::<Vec<(usize,usize,i32)>>(),
    ///     vec![(0,0,0),(1,0,0),(1,1,0),(2,0,0),(2,1,42),(2,2,0)]);
	/// ```
	#[inline(always)]
    pub fn enumerate_items(&'a self) -> impl Iterator<Item=(usize, usize, &V)> {
        let level_count = self.level_count();
        self.vec.iter().enumerate().map(move |(v_idx,item)|
            match level_of(v_idx, level_count) {
                Ok((level, offset)) => (level, offset, item),
                Err(_) => panic!("logic error")
            }
        )
    }
	
	#[inline(always)]
	pub fn iter_mut(&mut self) -> impl Iterator<Item=&mut[V]> {
		AVecMutIter::new(self)
	}
	
	#[inline(always)]
	pub fn iter(&self) -> impl Iterator<Item=&'a [V]>  {
		AVecIter::new(self)
	}

	#[inline(always)]
    pub fn iter_items_mut(&mut self) -> impl Iterator<Item=&mut V> {
        self.vec.iter_mut()
    }
	
	#[inline(always)]
    pub fn iter_items(&self) -> impl Iterator<Item=& V> {
        self.vec.iter()
    }
	
	#[inline(always)]
	pub fn levels(&self) -> impl Iterator<Item=&[V]> {
		self.iter()
	}
	
	#[inline(always)]
	pub fn level_count(&self) -> usize {
		self.levels
	}

	#[inline(always)]
	#[allow(clippy::same_item_push)]
	pub fn reserve(&mut self, levels: usize) {
		if self.levels <= levels {
			self.vec.resize_with(arithmetic_idx(levels+1,0), V::default);
			self.levels = levels+1;
		}
	}
}

impl<'a, V:'a+Default> Default for ArithmeticVec<'a, V> {
	/// ```
	/// # use arithmetic_vec::*;
	/// assert!(ArithmeticVec::<i32>::default().level_count() == 0); 
	/// ```
	fn default() -> Self {
		Self::with_capacity(10)
	}
}

impl<'a, V:'a+Default> Debug for ArithmeticVec<'a, V> {
	fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
		todo!()
	}
}

impl<'a, V:'a+Default> Index<usize> for ArithmeticVec<'a, V> {
	type Output = [V];
	
	/// ```
	/// # use arithmetic_vec::*;
	/// let mut av = ArithmeticVec::default();
	/// av[(2,1)] = 42;
	/// assert!(av[(2,1)] == 42);
    /// assert!(av[0] == [0]);
    /// assert!(av[1] == [0,0]);
	/// assert!(av[2] == [0,42,0]);
    /// av[(999,999)] = 42;
    /// assert!(av[999].len() == 1000);
    /// assert!(av[999][999] == 42);
	/// ```
	#[inline]
	fn index(&self, level: usize) -> &[V] {
		let range = arithmetic_idx(level,0) .. arithmetic_idx(level+1,0);
		&self.vec[range]
	}
}

impl<'a, V:'a+Default> IndexMut<usize> for ArithmeticVec<'a, V> {
	/// ```
	/// # use arithmetic_vec::*;
	/// let mut av = ArithmeticVec::default();
	/// av[2][1] = 42;
	/// assert!(av[(2,1)] == 42);
	/// assert!(av[2] == [0,42,0]);
	/// let mut _av_lvl_3 : &mut[i32] = &mut av[2];
	/// ```
	#[inline]
	fn index_mut(&mut self, level: usize) -> &mut [V] {
		self.reserve(level);
		let range = arithmetic_idx(level,0) .. arithmetic_idx(level+1,0);
		&mut self.vec[range]
	}
}

impl<'a, V:'a+Default> Index<(usize,usize)> for ArithmeticVec<'a, V> {
	type Output = V;
	#[inline]
	fn index(&self, (level,idx): (usize,usize)) -> &V {
		&self.vec[arithmetic_idx(level,idx)]
	}
}

impl<'a, V:'a+Default> IndexMut<(usize,usize)> for ArithmeticVec<'a, V> {
	#[inline]
	fn index_mut(&mut self, (level,idx): (usize,usize)) -> &mut V {
		self.reserve(level);
		&mut self.vec[arithmetic_idx(level,idx)]
	}
}

impl<'a, V:'a+Default> AsRef<ArithmeticVec<'a, V>> for ArithmeticVec<'a, V> {
	fn as_ref(&self) -> &ArithmeticVec<'a, V> {
		self
	}
}

impl<'a, V:'a+Default> AsMut<ArithmeticVec<'a, V>> for ArithmeticVec<'a, V> {
	fn as_mut(&mut self) -> &mut ArithmeticVec<'a, V> {
		self
	}
}

impl<'a, 'b, A, B> PartialEq<ArithmeticVec<'b, B>> for ArithmeticVec<'a, A>
where
	A: 'a + Default + PartialEq<B>,
	B: 'b + Default
{
	fn eq(&self, rhs: &ArithmeticVec<B>) -> bool {
		self.vec.eq(&rhs.vec)
	}
}

impl<'a, V:'a+Default+Eq> Eq for ArithmeticVec<'a, V> {}

impl<'a, V:'a+Default> PartialOrd<ArithmeticVec<'a, V>> for ArithmeticVec<'a, V>
where
	V: 'a + Default + PartialOrd<V>
{
	fn partial_cmp(&self, rhs: &ArithmeticVec<V>) -> Option<Ordering> {
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

impl<'a, V> Ord for ArithmeticVec<'a, V>
where
	V: 'a + Default + Ord
{
	fn cmp(&self, rhs: &ArithmeticVec<V>) -> Ordering {
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

impl<'a, V> Hash for ArithmeticVec<'a, V>
where
	V: 'a + Default + Hash
{
	fn hash<H: Hasher>(&self, state: &mut H) {
		Hash::hash(&self.vec, state);
		Hash::hash(&self.levels, state);
	}
}

pub struct AVecIter<'a, V:'a+Default> {
	cur_ptr: NonNull<V>,
	cur: usize,
	len: usize,
	_phantom: PhantomData<&'a V>,
}

impl<'a, V:'a+Default> AVecIter<'a, V> {
	pub fn new(source: &ArithmeticVec<'a, V>) -> Self {
		AVecIter {
			cur_ptr: NonNull::new(source.vec.as_ptr() as *mut V).unwrap(),
			cur: 0,
			len: source.levels,
			_phantom: PhantomData
		}
	}
}

impl<'a, V:'a+Default> Iterator for AVecIter<'a, V> {
	type Item = &'a[V];
	
	#[inline]
	fn next(&mut self) -> Option<Self::Item> {
		debug_assert!(self.cur <= self.len);
		if self.cur == self.len {
			return None
		}
		self.cur += 1; // pre-increment because each level has level+1 elements
		unsafe {
            let slice = slice::from_raw_parts(self.cur_ptr.as_ptr(), self.cur);
			self.cur_ptr = NonNull::new_unchecked(self.cur_ptr.as_ptr().add(self.cur));
			Some(&slice)
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

impl<'a, V:'a+Default> AVecMutIter<'a, V> {
	pub fn new(source: &mut ArithmeticVec<'a, V>) -> Self {
		AVecMutIter {
			cur_ptr: NonNull::new(source.vec.as_mut_ptr()).unwrap(),
			cur: 0,
			len: source.levels,
			_phantom: PhantomData
		}
	}
}

impl<'a, V:'a+Default> Iterator for AVecMutIter<'a, V> {
	type Item = &'a mut[V];
	
	#[inline]
	fn next(&mut self) -> Option<Self::Item> {
		debug_assert!(self.cur <= self.len);
		if self.cur == self.len {
			return None
		}
		self.cur += 1; // pre-increment because each level has level+1 elements
		unsafe {
            let slice = slice::from_raw_parts_mut(self.cur_ptr.as_ptr(), self.cur);
			self.cur_ptr = NonNull::new_unchecked(self.cur_ptr.as_ptr().add(self.cur));
			Some(slice)
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
impl<'a, V:'a+Default> ExactSizeIterator for AVecMutIter<'a, V> {
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
