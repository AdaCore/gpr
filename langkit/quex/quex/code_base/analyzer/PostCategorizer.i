/* -*- C++ -*- vim: set syntax=cpp: */
#ifndef __QUEX_INCLUDE_GUARD__ANALYZER__POST_CATEGORIZER_I
#define __QUEX_INCLUDE_GUARD__ANALYZER__POST_CATEGORIZER_I
#ifdef  QUEX_OPTION_POST_CATEGORIZER

#include <quex/code_base/MemoryManager>
#include <quex/code_base/analyzer/PostCategorizer>
#include <quex/code_base/aux-string>

QUEX_NAMESPACE_MAIN_OPEN

QUEX_INLINE void
QUEX_NAME(PostCategorizer_clear_recursively)(QUEX_NAME(Dictionary)*      me, 
                                             QUEX_NAME(DictionaryNode)*  branch);

QUEX_INLINE  QUEX_NAME(DictionaryNode)*  
QUEX_NAME(PostCategorizer_allocate_node)(size_t RemainderL)
{
    /* Allocate in one beat: base and remainder: 
     *
     *   [Base   |Remainder             ]
     *
     * Then bend the base->name_remainder to the Remainder part of the allocated
     * memory. Note, that this is not very efficient, since one should try to allocate
     * the small node objects and refer to the remainder only when necessary. This
     * would reduce cache misses.                                                      */
    const size_t   BaseSize      = sizeof(QUEX_NAME(DictionaryNode));
    /* Length + 1 == memory size (terminating zero) */
    const size_t   RemainderSize = sizeof(QUEX_TYPE_CHARACTER) * (RemainderL + 1);
    uint8_t*       base          = 
                      (uint8_t*)
                      QUEXED(MemoryManager_allocate)(BaseSize + RemainderSize, 
                                                     QUEXED(MemoryObjectType_POST_CATEGORIZER_NODE));
    ((QUEX_NAME(DictionaryNode)*)base)->name_remainder = (const QUEX_TYPE_CHARACTER*)(base + BaseSize);
    return (QUEX_NAME(DictionaryNode)*)base;
}

QUEX_INLINE  void 
QUEX_NAME(PostCategorizer_free_node)(QUEX_NAME(DictionaryNode)* node)
{ 
    if( node == 0x0 ) return;
    
    QUEXED(MemoryManager_free)((void*)node, 
                               QUEXED(MemoryObjectType_POST_CATEGORIZER_NODE)); 
}

QUEX_INLINE QUEX_NAME(DictionaryNode)* 
QUEX_NAME(DictionaryNode_new)(QUEX_TYPE_CHARACTER         FirstCharacter,
                              const QUEX_TYPE_CHARACTER*  Remainder,
                              QUEX_TYPE_TOKEN_ID          TokenID)
{
    QUEX_NAME(DictionaryNode)* me = QUEX_NAME(PostCategorizer_allocate_node)(QUEX_NAME(strlen)(Remainder));
    me->name_first_character = FirstCharacter;
    me->name_remainder       = Remainder;
    me->token_id             = TokenID;
    me->lesser  = 0x0;
    me->greater = 0x0;
    return me;
}

QUEX_INLINE void
QUEX_NAME(PostCategorizer_construct)(QUEX_NAME(Dictionary)* me)
{
    me->root = 0x0;
}

QUEX_INLINE void
QUEX_NAME(PostCategorizer_destruct)(QUEX_NAME(Dictionary)* me)
{
    if( me->root == 0x0 ) return;
    QUEX_NAME(PostCategorizer_clear_recursively)(me, me->root);
    me->root = 0x0;
}

QUEX_INLINE int
QUEX_NAME(PostCategorizer_compare)(QUEX_NAME(DictionaryNode)*        me, 
                                   QUEX_TYPE_CHARACTER        FirstCharacter, 
                                   const QUEX_TYPE_CHARACTER* Remainder)
    /* Returns: '0'   if both strings are the same
       '< 0' string 0 < string 1
       '> 0' string 0 > string 1           */
{
    const QUEX_TYPE_CHARACTER* it0 = 0x0;
    const QUEX_TYPE_CHARACTER* it1 = 0x0;

    if     ( FirstCharacter > me->name_first_character ) return 1;
    else if( FirstCharacter < me->name_first_character ) return -1;
    else {
        /* Implementation according to: P.J. Plauger, "The Standard C Library", 1992 */
        it0 = Remainder;
        it1 = me->name_remainder;
        for(; *it0 == *it1; ++it0, ++it1) {
            /* Both letters are the same and == 0?
             * => both reach terminall zero without being different. */
            if( *it0 == 0 ) return 0;
        }
        return (int)(*it0) - (int)(*it1);
    }
}

QUEX_INLINE void
QUEX_NAME(PostCategorizer_enter)(QUEX_NAME(Dictionary)* me,
                                 const QUEX_TYPE_CHARACTER*  EntryName, 
                                 const QUEX_TYPE_TOKEN_ID    TokenID)
{
    QUEX_TYPE_CHARACTER           FirstCharacter = EntryName[0];
    const QUEX_TYPE_CHARACTER*    Remainder = FirstCharacter == 0x0 ? 0x0 : EntryName + 1;
    QUEX_NAME(DictionaryNode)*    node      = me->root;
    QUEX_NAME(DictionaryNode)*    prev_node = 0x0;
    int                           result = 0;

    if( me->root == 0x0 ) {
        me->root = QUEX_NAME(DictionaryNode_new)(FirstCharacter, Remainder, TokenID);
        return;
    }
    while( node != 0x0 ) {
        prev_node = node;
        result    = QUEX_NAME(PostCategorizer_compare)(node, FirstCharacter, Remainder);
        if     ( result > 0 ) node = node->greater;
        else if( result < 0 ) node = node->lesser;
        else                  return; /* Node with that name already exists */
    }
    __quex_assert( prev_node != 0x0 );
    __quex_assert( result != 0 );

    if( result > 0 ) 
        prev_node->greater = QUEX_NAME(DictionaryNode_new)(FirstCharacter, Remainder, TokenID);
    else 
        prev_node->lesser  = QUEX_NAME(DictionaryNode_new)(FirstCharacter, Remainder, TokenID);
}

QUEX_INLINE void
QUEX_NAME(PostCategorizer_remove)(QUEX_NAME(Dictionary)*  me,
                                  const QUEX_TYPE_CHARACTER*   EntryName)
{
    int                               result = 0;
    QUEX_TYPE_CHARACTER               FirstCharacter = EntryName[0];
    const QUEX_TYPE_CHARACTER*        Remainder = FirstCharacter == 0x0 ? 0x0 : EntryName + 1;
    QUEX_NAME(DictionaryNode)*  node   = 0x0;
    QUEX_NAME(DictionaryNode)*  parent = 0x0;
    QUEX_NAME(DictionaryNode)*  found  = me->root;

    __quex_assert( found != 0x0 );
    while( 1 + 1 == 2 ) {
        result = QUEX_NAME(PostCategorizer_compare)(found, FirstCharacter, Remainder);

        /* result == 0: found's name == EntryName 
         * On 'break': If found == root then parent = 0x0 which triggers a special treatment. */
        if( result == 0 ) break;

        parent = found;

        if     ( result > 0 )  found = found->greater;
        else if( result < 0 ) found = found->lesser;

        if( found == 0x0 ) return; /* Not found name with that name */
    };
    /* Found a node with 'EntryName' */

    /* Remove node and re-order tree */
    if( parent == 0x0 ) {
        if( found->lesser != 0x0 ) {
            for(node = found->lesser; node->greater != 0x0; node = node->greater );
            node->greater = found->greater;
            me->root      = found->lesser;
        } else {
            me->root      = found->greater;
        }
    }
    else if( found == parent->lesser ) {
        /* (1) 'found' is the 'lesser' child of the parent:
         *
         *                 (parent's greater tree)
         *                /
         *        (parent)        (greater tree)
         *               \       /
         *                (found)
         *                       \
         *                        (lesser tree)
         *
         *     All subnodes of (greater tree) are greater than all subnodes in (lesser tree).
         *     => (i) mount (lesser tree) to the least node of (greater tree).                
         *     Anything in the subtree of 'found' is lesser than anything in 'parent's 
         *     greater tree.
         *     => (ii) mount (greater tree) to the least node of the (parent's greater tree). */
        /* parent != 0x0, see above */
        if( found->greater != 0x0 ) {
            for(node = found->greater; node->lesser != 0x0; node = node->lesser );
            node->lesser   = found->lesser;
            parent->lesser = found->greater;
        } else {
            parent->lesser = found->lesser;
        }

    } else {
        /* (2) 'found' is the 'greater' child of the parent:
         *
         *     (i)  mount (greater tree) to the greatest node of (greater tree).                  
         *     (ii) mount (lesser tree) to the greatest node of the (parent's lesser tree). */
        /* parent != 0x0, see above */
        if( found->lesser != 0x0 ) {
            for(node = found->lesser; node->greater != 0x0; node = node->greater );
            node->greater   = found->greater;
            parent->greater = found->lesser;
        } else {
            parent->greater = found->greater;
        }
    }
    QUEX_NAME(PostCategorizer_free_node)(found);
}

QUEX_INLINE QUEX_NAME(DictionaryNode)*
QUEX_NAME(PostCategorizer_find)(const QUEX_NAME(Dictionary)*  me, 
                                const QUEX_TYPE_CHARACTER*         EntryName)
{
    QUEX_TYPE_CHARACTER               FirstCharacter = EntryName[0];
    const QUEX_TYPE_CHARACTER*        Remainder      = FirstCharacter == 0x0 ? 0x0 : EntryName + 1;
    QUEX_NAME(DictionaryNode)*  node           = me->root;

    while( node != 0x0 ) {
        int result = QUEX_NAME(PostCategorizer_compare)(node, FirstCharacter, Remainder);

        if     ( result > 0 ) node = node->greater;
        else if( result < 0 ) node = node->lesser;
        else                  return node;
    }
    return 0x0;
}

QUEX_INLINE void
QUEX_NAME(PostCategorizer_clear_recursively)(QUEX_NAME(Dictionary)*       me, 
                                             QUEX_NAME(DictionaryNode)*  branch)
{
    __quex_assert(branch != 0x0);

    if( branch->lesser  != 0x0 ) QUEX_NAME(PostCategorizer_clear_recursively)(me, branch->lesser);
    if( branch->greater != 0x0 ) QUEX_NAME(PostCategorizer_clear_recursively)(me, branch->greater);
    QUEX_NAME(PostCategorizer_free_node)(branch);
}

QUEX_INLINE QUEX_TYPE_TOKEN_ID 
QUEX_NAME(PostCategorizer_get_token_id)(const QUEX_NAME(Dictionary)*  me,
                                        const QUEX_TYPE_CHARACTER*   Lexeme)
{
    QUEX_NAME(DictionaryNode)* found = QUEX_NAME(PostCategorizer_find)(me, Lexeme);
    if( found == 0x0 ) return __QUEX_SETTING_TOKEN_ID_UNINITIALIZED;
    return found->token_id;
}

    QUEX_INLINE void
QUEX_NAME(PostCategorizer_clear)(QUEX_NAME(Dictionary)* me)
{
    if( me->root == 0x0 ) return;
    QUEX_NAME(PostCategorizer_clear_recursively)(me, me->root);
}

    QUEX_INLINE void
QUEX_NAME(PostCategorizer_print_tree)(QUEX_NAME(DictionaryNode)* node, int Depth)
{
    int i = 0;
    if( node == 0x0 ) {
        for(i=0; i<Depth; ++i) __QUEX_STD_printf("        ");
        __QUEX_STD_printf("[EMPTY]\n");
        return;
    }

    QUEX_NAME(PostCategorizer_print_tree)(node->greater, Depth + 1);

    for(i=0; i < Depth + 1; ++i) __QUEX_STD_printf("        ");
    __QUEX_STD_printf("/\n");

    for(i=0; i<Depth; ++i) __QUEX_STD_printf("        ");
    {
        uint8_t  drain[256];
        uint8_t* drain_p = &drain[0];
        uint8_t* remainder_p = (uint8_t*)0; 
        const QUEX_TYPE_CHARACTER* source_p     = &node->name_first_character;
        const QUEX_TYPE_CHARACTER* source_end_p = &source_p[1];

        /* Convert the first character                                       */
        QUEX_NAME(to_utf8)(&source_p, source_end_p, &drain_p, &drain[256]);

        *drain_p++   = '\0';
        remainder_p  = drain_p;
        source_p     = node->name_remainder;
        source_end_p = source_p + QUEX_NAME(strlen)(source_p) + 1;

        /* Convert the remainder                                             */
        QUEX_NAME(to_utf8)(&source_p, source_end_p, &drain_p, &drain[256]);

        __QUEX_STD_printf("[%s]%s: %i\n", &drain[0], remainder_p, 
                          (int)node->token_id);
    }

    for(i=0; i<Depth + 1; ++i) __QUEX_STD_printf("        ");
    __QUEX_STD_printf("\\\n");

    QUEX_NAME(PostCategorizer_print_tree)(node->lesser, Depth + 1);
}

    QUEX_INLINE void
QUEX_NAME(PostCategorizer_print_this)(QUEX_NAME(Dictionary)* me)
{
    QUEX_NAME(PostCategorizer_print_tree)(me->root, 0);
}


#ifndef __QUEX_OPTION_PLAIN_C
QUEX_INLINE void
QUEX_NAME(Dictionary)::clear()
{ QUEX_NAME(PostCategorizer_clear)(this); }

QUEX_INLINE QUEX_TYPE_TOKEN_ID 
QUEX_NAME(Dictionary)::get_token_id(const QUEX_TYPE_CHARACTER* Lexeme) const
{ return QUEX_NAME(PostCategorizer_get_token_id)(this, Lexeme); }

QUEX_INLINE void
QUEX_NAME(Dictionary)::remove(const QUEX_TYPE_CHARACTER* EntryName)
{ QUEX_NAME(PostCategorizer_remove)(this, EntryName); }

QUEX_INLINE void
QUEX_NAME(Dictionary)::enter(const QUEX_TYPE_CHARACTER*  EntryName, 
                             const QUEX_TYPE_TOKEN_ID    TokenID)
{ QUEX_NAME(PostCategorizer_enter)(this, EntryName, TokenID); }

QUEX_INLINE void
QUEX_NAME(Dictionary)::print_this()
{ QUEX_NAME(PostCategorizer_print_this)(this); }

#endif 

QUEX_NAMESPACE_MAIN_CLOSE

#include <quex/code_base/aux-string.i>

#endif /* QUEX_OPTION_POST_CATEGORIZER */
#endif /* __QUEX_INCLUDE_GUARD__ANALYZER__POST_CATEGORIZER_I */
