/*---------------------------------------------------------------------------
 * Wrapper for the system's malloc() routines
 *
 *---------------------------------------------------------------------------
 */

#include "driver.h"

#include <stdlib.h>

#include "sysmalloc.h"

#include "mstrings.h"
#include "stdstrings.h"
#include "svalue.h"

#include "../mudlib/sys/driver_info.h"

/* Defines required by the xalloc.c wrapper */
/* #undef REPLACE_MALLOC */
#define NO_MEM_BLOCK_SIZE
#define MEM_THREADSAFE

/*-------------------------------------------------------------------------*/

static char * heap_start = NULL;
static char * heap_end = NULL;
  /* The start and end of the heap, as we know it.
   */

/*-------------------------------------------------------------------------*/
static inline void *
mem_alloc (size_t size)

/* Allocate a memory block for <size> bytes at the source <file>:<line>.
 * Result is the pointer the memory block, or NULL when out of memory.
 */

{
    void * rc;

    rc = malloc(size);

    if (heap_start == NULL || (char *)rc < heap_start)
        heap_start = rc;
    if (heap_end == NULL || (char *)rc + size > heap_end)
        heap_end = rc + size;

    assert_stack_gap();

    return rc;
} /* mem_alloc() */

/*-------------------------------------------------------------------------*/
static INLINE void
mem_free (void * ptr)

/* Return the memoryblock <ptr> to the allocator.
 */

{
    free(ptr);
} /* mem_free() */

/*-------------------------------------------------------------------------*/
static void *
mem_realloc (void * p, size_t size)

/* Reallocate block <p> to the new size of <size> and return the pointer.
 */

{
    return realloc(p, size);
} /* mem_realloc() */

/*-------------------------------------------------------------------------*/
static INLINE void
mem_mark_permanent (void * p UNUSED)

/* Mark the allocated block at <p> as permanent, ie. it won't be subject
 * to the GC.
 */

{
#   ifdef __MWERKS__
#      pragma unused(p)
#   endif
    /* Nothing */
} /* mem_mark_permanent() */

/*-------------------------------------------------------------------------*/
static INLINE void
mem_mark_collectable (void * p UNUSED)

/* Mark the allocated block at <p> as non-permant, ie. it is subject
 * to the GC.
 */

{
#   ifdef __MWERKS__
#      pragma unused(p)
#   endif
    /* Nothing */
} /* mem_mark_collectable() */

/*-------------------------------------------------------------------------*/
static INLINE size_t
mem_block_size (void * p UNUSED)

/* Return the size of block <p> (sans internal overhead) in bytes.
 */

{
#   ifdef __MWERKS__
#      pragma unused(p)
#   endif
   return 0;
} /* mem_block_size() */

/*-------------------------------------------------------------------------*/
static INLINE size_t
mem_overhead (void)

/* Return the size of each block's overhead in bytes.
 */

{
   return EXTERN_MALLOC_OVERHEAD;
} /* mem_overhead() */

/*-------------------------------------------------------------------------*/
static INLINE void *
mem_increment_size (void *vp, size_t size)

/* Try to extent the allocation block for <vp> to hold <size> more bytes.
 * If this is not possible, return NULL, otherwise return a pointer
 * to the start of the block extension.
 */

{
    return NULL;
} /* mem_increment_size() */

/*-------------------------------------------------------------------------*/
void
mem_dump_data (strbuf_t *sbuf)

/* For the status commands and functions: add the smalloc statistic
 * to the buffer <sbuf>.
 */

{
    strbuf_add(sbuf, "Using system standard malloc.\n");
    strbuf_addf(sbuf,
                "soft memory limit: %10lu, hard memory limit: %10lu\n\n",
                get_memory_limit(MALLOC_SOFT_LIMIT),
                get_memory_limit(MALLOC_HARD_LIMIT)
               );

} /* mem_dump_data() */

/*-------------------------------------------------------------------------*/
void
mem_driver_info (svalue_t *svp, int value)

/* Returns the memory information for driver_info(<what>).
 * <svp> points to the svalue for the result.
 */

{
    switch (value)
    {
        case DI_MEMORY_ALLOCATOR_NAME:
            put_ref_string(svp, STR_SYSTEM_MALLOC);
            break;

        case DI_NUM_SYS_ALLOCATED_BLOCKS:
        case DI_NUM_LARGE_BLOCKS_ALLOCATED:
        case DI_NUM_LARGE_BLOCKS_FREE:
        case DI_NUM_LARGE_BLOCKS_WASTE:
        case DI_NUM_SMALL_BLOCKS_ALLOCATED:
        case DI_NUM_SMALL_BLOCKS_FREE:
        case DI_NUM_SMALL_BLOCKS_WASTE:
        case DI_NUM_SMALL_BLOCK_CHUNKS:
        case DI_NUM_UNMANAGED_BLOCKS:
        case DI_NUM_FREE_BLOCKS_AVL_NODES:
        case DI_SIZE_SYS_ALLOCATED_BLOCKS:
        case DI_SIZE_LARGE_BLOCKS_ALLOCATED:
        case DI_SIZE_LARGE_BLOCKS_FREE:
        case DI_SIZE_LARGE_BLOCKS_WASTE:
        case DI_SIZE_LARGE_BLOCK_OVERHEAD:
        case DI_SIZE_SMALL_BLOCKS_ALLOCATED:
        case DI_SIZE_SMALL_BLOCKS_FREE:
        case DI_SIZE_SMALL_BLOCKS_WASTE:
        case DI_SIZE_SMALL_BLOCK_OVERHEAD:
        case DI_SIZE_SMALL_BLOCK_CHUNKS:
        case DI_SIZE_UNMANAGED_BLOCKS:
        case DI_SIZE_MEMORY_USED:
        case DI_SIZE_MEMORY_UNUSED:
        case DI_SIZE_MEMORY_OVERHEAD:
        case DI_NUM_INCREMENT_SIZE_CALLS:
        case DI_NUM_INCREMENT_SIZE_CALL_SUCCESSES:
        case DI_SIZE_INCREMENT_SIZE_CALL_DIFFS:
        case DI_NUM_REPLACEMENT_MALLOC_CALLS:
        case DI_SIZE_REPLACEMENT_MALLOC_CALLS:
        case DI_NUM_MEMORY_DEFRAGMENTATION_CALLS_FULL:
        case DI_NUM_MEMORY_DEFRAGMENTATION_CALLS_TARGETED:
        case DI_NUM_MEMORY_DEFRAGMENTATION_CALL_TARGET_HITS:
        case DI_NUM_MEMORY_DEFRAGMENTATION_BLOCKS_INSPECTED:
        case DI_NUM_MEMORY_DEFRAGMENTATION_BLOCKS_MERGED:
        case DI_NUM_MEMORY_DEFRAGMENTATION_BLOCKS_RESULTING:
        case DI_MEMORY_EXTENDED_STATISTICS:
            put_number(svp, 0);
            break;

        default:
            fatal("Unknown option for mem_driver_info(): %d\n", value);
            break;
    }

} /* mem_driver_info() */


/*-------------------------------------------------------------------------*/
void
mem_dump_extdata (strbuf_t *sbuf)

/* For the status commands and functions: add the extended smalloc statistic
 * to the buffer <sbuf>.
 */

{
    strbuf_add(sbuf, "No detailed blocks statistics available.\n");
} /* mem_dump_extdata() */

/*-------------------------------------------------------------------------*/
Bool
mem_dump_memory (int fd)

/* Print the location, size, and (if available) the TRACE information
 * of all memory blocks to file <fd>, and return TRUE.
 * If the allocator doesn't support this operation, print nothing
 * and return FALSE.
 */

{
    return MY_FALSE;
} /* mem_dump_memory() */

/*-------------------------------------------------------------------------*/
void
mem_consolidate (Bool force UNUSED)

/* Consolidate the free small blocks, merging them into larger free blocks
 * where possible, and rebuild the free lists.
 */

{
#   ifdef __MWERKS__
#      pragma unused(force)
#   endif
    /* Nothing */
} /* mem_consolidate() */

/*-------------------------------------------------------------------------*/
#ifdef MALLOC_EXT_STATISTICS
void
mem_update_stats (void)

/* Update whatever extended statistics the allocator has. Called every
 * backend cycle or so to allow for the calculation of averages over time.
 */

{
    /* Nothing */
} /* mem_update_stats() */
#endif /* MALLOC_EXT_STATISTICS */

/*-------------------------------------------------------------------------*/
static INLINE p_int
mem_mem_allocated()
/* The amount of memory currently allocated from the allocator, including 
 * the overhead for the allocator.
 */
{
    return 0;
}

/*-------------------------------------------------------------------------*/
static INLINE p_int
mem_mem_used()
/* The amount of memory currently used for driver data, excluding the 
 * overhead from the allocator.
 */
{
    return 0;
}

/***************************************************************************/
