/* Written by Nathan Addy.
This is a library of functions for the Smoldyn program.  See documentation
called Smoldyn_doc1.doc and Smoldyn_doc2.doc.
Copyright 2003-2007 by Steven Andrews.  This work is distributed under the terms
of the Gnu Lesser General Public License (LGPL). */

#include "smoldyn.h"
#include "smoldyn_config.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#ifdef THREADING
#include <pthread.h>
#endif


void double_stack_memory(stack* pStack);
void reset_stack_memory(stack* pStack);

// alloc_stack.  Returns a new stack whole hog.  
stack* alloc_stack()
{
    stack* new_stack = malloc( sizeof(struct stackstruct) );

    size_t total_size = BASE_STACK_SIZE;

    new_stack->stack_data = malloc( total_size );
    new_stack->max_size = total_size;
    new_stack->current_size = 0;

    return new_stack;
}

//free_stack.  
void free_stack(stack* pStack)
{
    free(pStack->stack_data);
    free(pStack);
}


// double_stack_memory.  Takes a stack pointer and doubles its
// internal memory.  
void double_stack_memory(stack* pStack)
{

    void* new_stack_mem = malloc(pStack->max_size * 2);
    void* old_stack_mem = pStack->stack_data;

    memcpy(new_stack_mem, pStack->stack_data, pStack->max_size);


    
    free( old_stack_mem );

    pStack->stack_data = new_stack_mem;
    pStack->max_size *= 2;
    
    return;
}

// reset_stack_memory.  Clears the stack, freeing its memory and resetting it to its
// original stack size.  Returns 0 for success, 1 for unknown error.
void reset_stack_memory(stack* pStack)
{

    size_t new_stack_size = BASE_STACK_SIZE;

    void* new_stack_mem = malloc( new_stack_size );
    void* old_stack_mem = pStack->stack_data;
    
    memcpy(new_stack_mem, pStack->stack_data, pStack->max_size);
    
    free( old_stack_mem );

    pStack->stack_data = new_stack_mem;
    pStack->max_size = new_stack_size;
    
    return;
}


// push_data_onto_stack.  Given a stack pointer, an address of data, and a specification
// of how many bytes to copy, will copy that data onto the stack.
void push_data_onto_stack(stack* pStack, void* data, size_t data_size)
{
    while( pStack->max_size < pStack->current_size + data_size)
	double_stack_memory( pStack);

    memcpy(pStack->stack_data + pStack->current_size, data, data_size);
    pStack->current_size += data_size;
}


// clear_stack.  Blows out the stack data, while keeping its data size.  
inline void clear_stack(stack* pStack)
{
    pStack->current_size = 0;
}

int getIndexWithGreatestInitialValue(int** list_to_balance, int number_of_sets)
{
    int ndx;
    int max_ndx = 0;
    int max_value = list_to_balance[0][0];

    for(ndx = 1; ndx != number_of_sets; ++ndx)
    {
	if (list_to_balance[ndx][0] > max_value)
	{
	    max_ndx = ndx;
	    max_value = list_to_balance[ndx][0];
	}
    }
    return max_ndx;
}

void rebalance_list(int* list_to_rebalance, int size_of_list)
{
    int ndx = 0;
    int sum = 0;

    while( list_to_rebalance[ndx] > 0 )
    {
	sum += list_to_rebalance[ndx++];
    }

    int av_size = sum / (ndx + 1);

    int new_ndx = 0;
    while( sum > 0)
    {
	if (new_ndx == ndx)
	{
	    list_to_rebalance[new_ndx] = sum;
	    sum = 0;
	}
	else
	{
	    list_to_rebalance[ new_ndx++ ] = av_size;
	    sum -= av_size;
	}
    }
}


void balance( int** list_to_balance, int* sizes_of_dataset, int number_of_sets , int max_threads)
{
    int iters;

    for(iters = 0; iters != max_threads; ++iters)
    {
	rebalance_list( list_to_balance[ getIndexWithGreatestInitialValue(list_to_balance, number_of_sets) ], max_threads);
    }
}


threadptr alloc_thread()
{
    threadptr thread = malloc(sizeof(struct threadstruct));
    
#ifdef THREADING
    thread->thread_id = malloc(sizeof(pthread_t));
#else
    thread->thread_id = NULL;
#endif

    thread->input_stack = alloc_stack();
    thread->output_stack = alloc_stack();
    
    return thread;
}

void clearthreaddata( threadptr pthread)
{
    clear_stack( pthread->input_stack);
    clear_stack( pthread->output_stack);
}

int calculatestride(int total_number, int number_threads)
{
    if(total_number < number_threads) return 0;

    if( total_number % number_threads) return (total_number + (number_threads - (total_number % number_threads))) / number_threads;  
    else return total_number / number_threads;
}


// alloc_threadss.  Allocate a thread superstructure with the given number of threads.   
threadssptr alloc_threadss(int numberThreads)
{
    int thread_ndx;
    threadssptr new_threads = malloc(sizeof(struct threadingsuperstruct));

    new_threads->nthreads = numberThreads;

    new_threads->thread = malloc( numberThreads * sizeof(struct threadstruct));

    for(thread_ndx = 0; thread_ndx != numberThreads; ++thread_ndx)
    {
	new_threads->thread[thread_ndx] = alloc_thread();
    }
    
    return new_threads;
}



// free_thread.  Frees a specific thread structure (owned by the thread 
// superstructure and containing a thread id as well as other thread i/o data).  
void free_thread( threadptr thread)
{
    if( !thread ) return;

#ifdef THREADING
    free( (pthread_t*) thread->thread_id);
#endif

    free_stack( thread->input_stack );
    free_stack( thread->output_stack );

    free( thread );
}

// threadssfree.  Frees a threads superstructure.
void threadssfree(threadssptr threads)
{
    if(!threads) return;

    int thread_ndx; 

    for( thread_ndx = 0; thread_ndx != threads->nthreads; ++thread_ndx)
    {
	free_thread( threads->thread[thread_ndx]);
    }

    free( threads );
}



