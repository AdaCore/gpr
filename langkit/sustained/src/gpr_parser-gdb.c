
/* The following will ask GDB to load our GDB initialization script for Langkit
   GDB helpers.  Note that ASM bits are architecture-specific, so support only
   Linux for now.  */

#if defined(DEBUG) && defined(linux)
asm(
".pushsection \".debug_gdb_scripts\", \"MS\",@progbits,1\n"
".byte 1 /* Python */\n"
".asciz \"/home/pbeguet/WorkSpace/gpr_Issue#32/langkit/./generated/gdbinit.py\"\n"
".popsection\n"
);
#endif
