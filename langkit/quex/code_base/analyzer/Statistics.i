#ifndef __QUEX_INCLUDE_GUARD__ANALYZER__STATISTICS_I
#define __QUEX_INCLUDE_GUARD__ANALYZER__STATISTICS_I

#include <quex/code_base/analyzer/Statistics>

QUEX_INLINE void
QUEX_NAME(statistics_state_count)(const QUEX_NAME(statistics_state)* S, QUEX_TYPE_CHARACTER C)
{
    const QUEX_TYPE_CHARACTER*  BeginP   = S->interval_list.boundary;
    const QUEX_TYPE_CHARACTER*  EndP     = BeginP + S->interval_list.boundary_n;
    const QUEX_TYPE_CHARACTER*  low      = BeginP;
    const QUEX_TYPE_CHARACTER*  up       = EndP;
    const QUEX_TYPE_CHARACTER*  iterator = (const QUEX_TYPE_CHARACTER*)0x0; 

    /* Binary Search for the interval where 'C' belongs:
     * Find iterator so that: *(iterator-1) <= C < *(iterator) */
    while( up != low ) {
        iterator = low + ((up - low) >> 1);
        if( C < *iterator ) {
            if( iterator == BeginP )        break;
            else if( *(iterator - 1) <= C ) break; 
            up  = iterator;
        }
        else if( C >= *iterator ) { 
            if( iterator == EndP - 1 )      { iterator = EndP; break; }
            low = iterator;
        }
    }
    S->interval_list.counter[iterator - BeginP] += 1;
}

QUEX_INLINE void
QUEX_NAME(statistics_save)(const char* Filename)
{
    const QUEX_NAME(statistics_state)*  BeginP = (const QUEX_NAME(statistics_state)*)QUEX_NAME(statistics_state_list);
    const QUEX_NAME(statistics_state)*  EndP   = QUEX_NAME(statistics_state_list_end);
    const QUEX_NAME(statistics_state)*  s      = (const QUEX_NAME(statistics_state)*)0x0;
    FILE*                               fh     = __QUEX_STD_fopen(Filename, "w");
    size_t                              i      = 0;

    if( fh == NULL ) return;

    for(s = BeginP; s != EndP; ++s) {
        __QUEX_STD_fprintf(fh, "{\nmode: %s;\nstate: %i; {\n", (const char*)s->mode_name, (int)s->state_index);
        for(i = 0; i != s->interval_list.boundary_n; ++i) {
            __QUEX_STD_fprintf(fh, "%i ", (int)s->interval_list.boundary[i]);
        }
        __QUEX_STD_fprintf(fh, ";\n");
        for(i = 0; i != s->interval_list.boundary_n + 1; ++i) {
            __QUEX_STD_fprintf(fh, "%i ", (int)s->interval_list.counter[i]);
        }
        __QUEX_STD_fprintf(fh, ";\n}\n");
    }

    __QUEX_STD_fclose(fh);
}

#endif /* __QUEX_INCLUDE_GUARD__ANALYZER__STATISTICS_I */


