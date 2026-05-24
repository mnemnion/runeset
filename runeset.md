# Rune sets: Succinct Set Operations on UTF-8 Codepoints

This paper introduces the runeset: a data type supporting all set
operations over a generalization of the UTF-8 encoding.  It is succinct,
showing better compactness and speed when used with real Unicode
categories.  Performance on matching is excellent in modern hardware,
using as few as the theoretically minimum comparison operations.  Set
operations improve on linear performance over the number of elements:
set equality and subset relations are linear over the encoded runeset's
size.  Union, difference, and intersection take worst case O(n) time
for the number of elements, and this improves when we examine those
sets pre-defined by Unicode.  The encoding of the set's DFA employs
arithmetic operations for most transitions, improving over lookup-based
implementations in data locality and branch prediction, with a traversal
pattern which is strictly front-to-back, giving trivial prefetch.


## Structure and Function

A common techique for representing an ASCII character set uses two
64-bit unsigned integers.  The byte under test is shifted right six
places, and the values 2 and 3 rejected as not ASCII.  Values of 0 and 1
are tested against the low mask or high mask, respectively, by shifting
a `u64` with the value `1` by the value of the least six bits.  This
is then compared against the test mask in one of two equivalent ways:
either it is `&`ed and the result compared with `0`, or `|`ed and the
result compared with the mask.  For our purposes this test is true
when the mask bit is also `1`, although a mask complement is equally
effective for this simple test.

Runesets are a generalization of this technique.  They exploit a fact
about modern consumer-grade hardware which is relatively new: the
existence of single-instruction implementations of the Hamming count,
or popcount.  Advantage is taken of regularities in the UTF-8 encoding:
especially convenient is that every codeunit, one byte, is arranged so
that the low six bits carry the data for the code point, with the
high two indicating the category.  In order: low and high ASCII, follow
bytes, and lead bytes of any multi-byte codepoint.

The basis of UTF-8 is quite simple, but the actual implementation
is less so.  In order to meet the requirements of Unicode, there
are illegal byte patterns found in regions within the more general
encoding, and decoding must respect this, at least as far as detecting
and rejecting non-shortest encodings.  These are invalid code unit
sequences where, if the algorithm for adding up the value of a codepoint
is followed, will give a misleading result, by using several bytes
to represent a value whose shortest encoding uses fewer.  These pose
security risks, and it is always an error to decode them.  Conformant
decoders also treat sequences which would decode to a surrogate
codepoint as invalid, although sometimes this extension to the standard
is needed.

This makes decoding of UTF-8 unavoidably conditional, or 'branchy'.  We
have found that reducing the condition count using Bjorn Höhrmann's
lookup table algorithm roughly doubles speed relative to a more
straightforward detect-in-place, for some applications SIMD can be
applied for further speed, but the runeset technique does not call
for decoding the codeunit sequences in the first place.  Non-shortest
encodings will never be recognized as their shorter counterparts, and
better yet, runesets able to detect these encodings may be constructed.
Surrogate-equivalent encodings are equally able to be detected when this
affordance is useful.

The alphabet which the runeset constructs sets of, then, is this simpler
UTF-8 in its most general sense.  While it is possible to extend this
data structure to handle five and six byte encodings found in the
original definition, we have not found a reason to implement this
extension, and it will not be considered further.

A runeset constructed from a valid UTF-8 encoded string will never
match any sequence which is not valid, or any sequence representing a
codepoint not present in the construction string.


### Layout and Match Algorithm

A runeset consists of an array of `u64` integers.  No operation on a
properly constructed runeset is able to index beyond this array, so
it's safe to store this as only a pointer.

Conceptually this array is divided into four 'tiers', up to three of
which may not be present.  We will consider the complete case.  T₁
matches the first byte of any encoding, T₂, T₃, T₄, any subsequent
bytes.  These have subdivisions: to illustrate, T₂b is the part of
T₂ containing the second byte of encodings of two byte's length, and
T₂c the second byte of three-byte encodings.

T₁ is always present, and always consists of exactly four words.  The
first two match any ASCII characters which might be present, while the
third is the lead mask, matching the first byte of every multi-byte
encoding which is found in the set.  The fourth word is an optimization,
consisting of either `0`, or the offset of T₄ when such is present in
the set.

T₂ is `popcount(lead mask)` words in length, each of these words being a
follow-byte mask for all second bytes in a set encoding.  If a lead byte
matches on the lead mask, all bits higher than it in the lead mask are
masked off, and popcount used on the result to obtain the offset into
T₂.

The third byte is found in a similar fashion.  The second byte must by
construction be found in T₂c or T₂d, so values in T₂b are not of
interest: we mask off every bit _lower_ than our byte in the matching
T₂ mask, then take the popcount of the remainder, and the region
one past this word to the border of T₃, which is at at offset `4 +
popcount(lead mask)`.  This lays T₃ out backward: the bytes of the
highest-valued codepoints are found in the lower index masks, and those
of the lowest value still of three or four bytes in the higher ones.

As a consequence, the layout of T₄ becomes more complex.  To find
our match, we mask off the bits _higher_ than our match in T₃, and
add to this a popcount from the start of the T₃ to the word prior to
our matching mask.  T₃ is the length of the popcount of the c and d
regions of T₂, and we could use this calculation to find T₄, but
this is both somewhat expensive, and unchanging, so we cache that value
in T₁ instead.

This makes T₄'s layout 'striped', with each mask in T₃ defining a
stripe of T₄ which increases, while the stripes themselves decrease
in codepoint value.  This is irrelevant to matching performance, as
we could instead mask off the _lower_ bits, and T₄ would be more
simply backward.  But counterintuitively, this choice simplifies the
construction of set operations; not in terms of performance, but
the implementation itself becomes easier to reason about.  Ignoring
endianness, we can treat each bit in T₃ as strictly decreasing from
the start of the tier, while the other convention means we must treat
the bits as 'striped' in increasing order.

However this choice is arbitrary, and either convention will do.

This gives the runeset the property that any word not in T₁ will have
at least one bit set.  This does not itself prove succinctness, but it
motivates presenting such a claim.  Presenting a proof of this claim is
moderately challenging, because Unicode is more than just an ordered
collection of codepoints: it includes hundreds of categorizations which
partition the space into overlapping sets.  Our claim rests on the
reasonable axiom that these canonical sets are the information-theoretic
basis of Unicode, this is comparable to compression algorithms which are
designed for human languages focusing on their performance _on_ human
languages, rather than a wider net of compressible information.

The reason this matters is easy to see: if we were to define the problem
differently, say, that our sets consist of completely random codepoints,
with the number of such points normally distributed around `0x10ffff
/ 2`, the runeset is narrowly outperformed by a simple bitvector of
`0x10ffff` bits.  The canonical sets have both a mean and a median much
smaller than this would indicate, and the data clusters quite markedly
in Unicode space: runesets become smaller when either of these things
is true.  Similarly, a balanced range tree is more compact than the
equivalent runeset when the set is largely contiguous, so we must show
that for the canonical sets this is untrue often enough to meet the
claim of closeness to optimality.


## Performance

When matched against a string known to be valid Unicode, each
byte participates in one conditional to determine its membership.
Additionally, for multibyte encodings, we must check how many bytes
of the string we must match, to determine if we are successful or
merely succeeding.  The algorithm may be written so this check happens
once, splitting into three variations after the lead byte is matched,
depending on whether one, two, or three more matches will be required.
For simplicity and flexibility, we leave this up to the optimizer.  As
the result of subsequent checks is completely determined by the first
one, the simplest of predictors will get it right.

This is just one more than the absolute minimum number of conditions
which must be checked in order to decode the codeunit sequence into
a codepoint.  A lookup-based decoder must check for the reject state
after each transition, just as we must check success or failure after
each mask.

Thus, the prelude to any data structure which determines membership in
a Unicode set by first decoding the point, has reached our conditional
count on the first action taken towards an actual membership query.
Granted that a decoder of pre-validated data may be more efficient,
we consider the comparable branch factor of matching with decoding
significant, as we have no need at all to do the latter.

The calculation of the value also involves a mask and a shift per byte,
not different in essence to those performed by our comparison.  This
pays even more of our freight, so to speak.

The popcounts are, on a modern computer, as cheap as any other simple
arithmetic.  Our composite counts over more than one word are contiguous
and readily vectorizable.  Our memory fetch pattern is also optimal, as
we read from the array in a pattern which, while not a strict traversal,
benefits most from the simple low-to-high prefetch which the MMU will
execute.

We do not need a separate condition to find the mask with which to match
the first byte, on computers with 256 bit SIMD registers.  We simply
gather the low and high mask, and the lead mask, leaving a third `u64`
region of the register blank, then shift the entire byte into another
such register, mask, and only then compare.

While known-good Unicode is more efficient to test, and many languages
will ensure this is the case before any string operations commence,
the overhead for handling the greater uncertainty of arbitrary byte
sequences is fairly minimal.  A decoding strategy must decide what to
do with invalid data, but our matching routine for arbitrary input
handles this simply as another failure to match.  This makes it readily
applicable to forensic applications, where searching for text within
binaries, or the raw data from a hard drive or other storage, is
commonplace.


### Set Operations

The runeset is well-suited to all of the basic set operations.  Equality
is simply a comparison of the length of the array, and then its contents
in the event these are equal.  Subset comparison easily short-
circuits if a single lead byte is found in the test and not in the base:
otherwise we iteratively find masks in the base case, determine their
equivalent in the test, and mask these with `Test & ~Base` to determine
if the difference is not `∅`.

A detailed description of union, difference, and intersection is not
here provided.  We may hope that the outline is clear, however: bitwise
set operations are performed to iterate along the sets, finding leaf
sets to mask together, and for subtractive operations, propagate
emptied sets backward to remove their leads from the earlier bitmasks.
Following our property that all masks not in T₁ have a bit set, a
final pass, which compresses the array by removing all zeroed masks,
results in our new set.

We treat runesets as an immutable data structure, generating a third,
new set, as the consequence of each operation.  By their nature,
mutating in-place would complicate the operation, while gaining little
to nothing.  As we will see, real-world runesets are quite small, and
set construction will often begin with runesets in static memory.

It does make them less suitable for certain applications: very
frequently adding and removing single codepoints, for example, would
incur an overhead which something like a hashed set of the codepoint
values would not.  We view this as of only theoretical significance,
as those few practical operations which could be done this way can
be handled in a more runeset-friendly manner if desired, for example
building up a string with the desired sequences and making a runeset
from it when they are acquired, which will deduplicate them.

In fact, this is a practical way to obtain a lexically-ordered
collection of all codepoints in a text: clone the text and construct a
runeset, then iterate the set to write them out to a new string.

Since the set of all generalized UTF-8 sequences is finite, a set
complement is finite also: start with the supremum, and take the
difference of the set to be complemented.  When matching, it is more
practical to simply reverse the interpretation of matching, at least for
a small set, although the performance of even very large runesets is
fairly good.  But even this operation can be useful as an intermediate
value when constructing some sets of interest, such as the set of
all assigned codepoints which are not letters.

It's valid to define the complement in term's of Unicode's valid
alphabet rather than our expanded one, simply by starting with that
set, rather than the largest constructable runeset.  For this reason
among others we do not directly define a set complement operation; this
also relieves the core library from the duty of providing a supremum to
complement against, or indeed, any runesets at all.  A companion library
is provided containing most Unicode data in runeset form.


## Comparison

There is no single best approach to matching some subset of Unicode
characters.  An optimized pattern matching library, such as a regex
engine or PEG VM, might want to employ a few strategies, rather than
just one.  As we'll see, the runeset should be frequently chosen among
them.

In current production systems, the task is handled in one of a few ways:
as a balanced tree of ranges, a lazily-constructed DFA, or multi-stage
lookup tries.  Let's examine those choices over several elaborated
examples.

For Unicode blocks, a range is hard to beat, as they're defined to be
contiguous in the allocation space, so each block is only one range.
but looking up a codepoint by block usually represents an error in the
program.  The Greek_and_Coptic block, as the name suggests, has more
than just Greek in it, including several unallocated codepoints, and
much that is Greek will not match this block, or it and Greek_Extended.

Even still: the Greek_and_Coptic runeset is seven words long.  All but
three lead bytes fail, and as these are two byte sequences, we get
through with only one popcount.  A range is certainly preferable for a
block like CJK_Unified_Ideographs_Extension_B, where the runeset is
684 words and the test is still a single range of values.

For a more realistic query like the Greek _script_, the ranges are
numerous, and our runeset is 26 words.  Often this kind of query is
handled with a lookup table, this is able to take any codepoint and
return its script category, as those are disjoint.  Such a lookup table
is large and makes poor use of memory, but for applications which
need to _categorize_ codepoints, not just match them, this cost is
acceptable: with all optimizations applied, the nearly-branchless
nature of the lookup appears optimal.

But a lookup table cannot answer the most-correct of these queries,
membership in the Greek script _with_ its extensions.  By definition, the
characters in the ScriptExtended data table belong properly to more than
one script, and a lookup table which could return every script which a
codepoint might belong to would be both enormous and inefficient.  The
runeset for this category is only 30 words, and a balanced range would
be quite branchy indeed.

Further, that near-branchlessness is an illusion: practically speaking,
text is in UTF-8, not in UTF-32, so the act of generating the
codepoint to lookup in the trie is itself branchy.  As we have already
demonstrated, the branch factor of decoding is comparable to that of
RuneSet matching.

Runesets remain compact for surprisingly large Unicode sets.  The
Egyptian_Heiroglpyhs set is only 87 words, for instance.  Runesets
take advantage of the density of normal Unicode data, while remaining
efficient and correct when, as is most often the case, dense does not
mean contiguous.

I was surprised to discover that the general category `Lu`, consisting
of all uppercase letters, is a mere 83 words, fitting into 11 typical
cache lines.  An interesting pattern shows up when examining the raw
data: several of the words have the value `0x5555555555555555`, or are
mostly 5s.  This is the mask pattern for all odd-numbered bytes in a
word, showing that several runs of alphabetic characters collate upper
and lower cases together.  A challenge for any range-based method, to
be sure!  Much of the Cyrillic alphabet is represented this way, making
this hardly an abstract or obscure consideration.

The direct construction of a DFA, generally as a lazy result of regular
expression compiling and matching, does give good performance when
implemented correctly.  It is also practical and natural to do so
using the native encoding, rather than imposing a decode step into the
process.  But this comes with considerable memory pressure, since to
bring down time-to-first-match, little state minimization or compression
is performed.  This is bad enough that engines generally limit the size
of the DFA cache, such that for some workloads the set DFA might be
repeatedly evicted and rebuilt, or the engine may bail to another
matching strategy.  Access is also quite poor, touching widely-dispersed
regions in an order the CPU cannot tell from a random one.

Set operations on a DFA are practical but not very set-like.  The union
of two DFAs will match anything matched by either pattern, but the quick
way duplicates all inner state, which would need to be re-minimized.  A
DFA represents a set of codepoints only by happenstance, while a runeset
is tailored to the purpose.  This has advantages, for example, a DFA can
match its set forward or backward, something runesets can only simulate
by rewinding to the start of each character and then matching forward.
Although the latter is hardly a recondite operation.

Research is in progress on an efficient method to generate a DFA
matching the same language as a runeset.  A useful observation here
is that the runeset may be thought of as a compressed form of such a
DFA, and that expansion can use CLZ and CTZ instructions to overlap
runs of zero, giving a more compact DFA at low expense compared to
general-purpose minimization.

The weakness of the runeset is in the very largest of categories.  The
Han script in extended form is 1605 words, a large majority of them with
all bits set.  It is nonetheless practical to match this set with a
Runeset, but a pattern matching library aiming for optimal performance
on every task should consider special-casing ideographs, using some
hybrid technique like a lookup table on a clamped range of values, or
a preliminary match of the first byte against the most plausible Han
values.  As detailed performance measurements have not yet been taken,
tuning good heuristics for the threshold at which to turn to another
method is left for future work.

Better still would be to obviate that need, by extending Runeset to
give equal performance regardless of the size of the set.  A proposal
for this extension follows.

We may achieve acceptable performance here by tuning the matching
algorithm, especially on chips with wide SIMD registers.  The
operation which becomes costly is the calculation of the T₃ mask,
and occasionally T₄, although the astral planes of Unicode are
sparsely populated.  These calculations are simply Hamming weights of a
memory-contiuous array of words; a SIMD register with 512 bits can both
fetch and sum eight of these in a single instruction, a fact which these
larger runesets can take advantage of, potentially well enough to give
acceptable performance in practice.  As a point of interest, reaching
the T₃ region can involve summing no more than 21 such words, this
being the number of valid lead bytes for characters of three and four
byte encoding in UTF-8.  Furthermore, while the Han set in particular
now contains many four byte "astral" codepoints, the summation needed
to find the T₄ mask does not need to take into account the very large
collection of T₃ masks for three-byte characters (in our nomenclature,
T₃c), only the T₃d region, which also weighs toward more efficiency
in practice than the sheer size of the runeset may suggest.

Another option would be a variant which optimizes this process by
storing an array of these sums.  This would mean a T₂ mask would need
only to count the greater-than bits in the mask itself, and add to
this a single value stored in an array the size of the T₂c and T₂d
regions.  The greatest possible value to store is 64 × 20, making 16
bit values much greater than sufficient for such an array.

The 0 entry of this array could hold an offset, such that subtracting
this number from the offset into the start of T₂c will give the number
1, and indexing the rest of the offset array with that number would give
the value of the byte weight of those parts of the region of interest
which are not themselves this first mask.  This value is equivalent to
masking the 3 and 4 byte leads off the lead mask and popcounting, so
this optimization, if it is one, is not significant.

A similar process could cache sums of the T₃d region needed to find
offsets into T₄: here the maximum value is 5, the number of valid
four-byte lead values, times 64², or 20,480: still comfortably within
the range of a u16, or in fact an i15.  The maximum number of entries is
just 5 × 64, or 320, needing 640 bytes of storage.  The beginning of
the T₃ would be subtracted from the mask offset to index this array,
this number would be already calculated in the process of reaching
that mask, so storing it separately would not be useful.  These values
would only be approached by fanciful sets, or complements which need
to include the unassigned codepoints, but this technique should give
consistent performance regardless of size, and that at a modest memory
overhead.  Since this overhead would grow with the size of the data,
such an enhanced runeset would no longer be succinct, but this quality
serves no purpose beyond admiration, and still belongs to the original
for whenever admiration might be more important than sheer speed.

The principle of data locality suggests the optimal place to store
the enhancment is between T₂ and T₃.  This would call for some
additional mild contortions, with several remedies too obvious to
consider in detail.  The compromise of simply appending it subsequent
to the final T₄ mask is simpler, and reasonable heuristics about
the actual behavior of memory caches suggest any difference should be
negligible.

The `u64` used to store the offset into T₄ is massively larger than
it needs to be; that currently empty space could be comfortably used
to store the additional offsets needed to find these cached values.
Splitting the data across four `u16` would mean no additional CPU
instructions (such as masking) would be involved, merely different
ones.


## Rune Maps

A runeset may also be used to map any matched codepoint to some
enumerated quality.  This can be achieved by indexing into a dense
array, at the cost of somewhat more popcounts, or spread out into
regions of 64 bytes per final mask word.  Enums of 16 or fewer values,
or four or fewer, can subdivide the bytes efficiently and take up less
space accordingly.  Non-final masks do not need to be represented, and
the arithmetic needed to find the map offset given the set offset is
quick and straightforward.

This can only be advantageous when first matching some codepoint is a
useful step.  For algorithms which need to determine a category for
any arbitrary codepoint, there are better data structures available.
Runemaps which map every codepoint to such an array remain a useful
primitive, since difference and intersection on a runemap with a runeset
will have a well-defined result (unions need two runemaps with identical
mappings for any codepoint found in both).

Here again there is a certain amount of Hamming summation involved
in finding the offset within the value array which would contain the
value corresponding to the matched codepoint.  And here again, a policy
of caching that sum would suffice: ASCII may be handled with direct
popcounts, and anything higher might simply subtract four from the
offset within the runeset, where the codeunit is matched, indexing into
an array containing the base offset of that mask's matches, and using
the now familiar mask and popcount to find the last offset into such an
array.  For enumerations larger (unlikely) or smaller (common) than a
single byte, a final shift to obtain the power of two actually used is
required as a final step.

Another space optimization may be had during construction: unlike the
conventional two-stage tries used in the classical Unicode algorithms,
non-member points do not need a place in the value array.  Lookup tries
are routinely compressed, at least through the expedient of hashing
each data block, and reusing ones which already exist: but since the
'holes' or default values are included, it's frequent that the only
result of this is to cache one copy of the "nothing here" run.

Using a 'cache' for offsets, as described above, also frees us from
the requirement that the mapped values be stored in order: only a
run corresponding to the set bit needs to be stored, and that may
be stored anywhere.  These runs may be constructed for each block,
and the existing value array searched for the presence of that run:
a classic triangular algorithm, and hence O(n²), but as we've seen,
the block count of runesets is frequently small, and especially for a
map constructed in advance of runtime, this is a practical algorithm
likely to give meaningful space savings.  The offsets are no longer a
cache if this technique is used, hence the scare quotes beginning this
paragraph.

Work is ongoing on the map enhancement to the runeset structure: it is
hoped that the result of memorizing popcounts for matching, and
providing an indexable array of map indices, will be efficient enough to
be a competitive structure for determinizing NFAs.

This approach is meaningfully similar to a compression of the classic
two-stage data trie: The runeset itself compresses the space only to
64-codepoint swathes containing at least one hit, and that compressed
representation is used to index into the second tier, where the mask-
and-popcount technique is used to skip representing empty slots a second
time over.

