/****************************************************************************
 *                           GPR2 PROJECT MANAGER                           *
 *                                                                          *
 *                    Copyright (C) 2019-2022, AdaCore                      *
 *                                                                          *
 * This library is free software;  you can redistribute it and/or modify it *
 * under terms of the  GNU General Public License  as published by the Free *
 * Software  Foundation;  either version 3,  or (at your  option) any later *
 * version. This library is distributed in the hope that it will be useful, *
 * but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- *
 * TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            *
 *                                                                          *
 * As a special exception under Section 7 of GPL version 3, you are granted *
 * additional permissions described in the GCC Runtime Library Exception,   *
 * version 3.1, as published by the Free Software Foundation.               *
 *                                                                          *
 * You should have received a copy of the GNU General Public License and    *
 * a copy of the GCC Runtime Library Exception along with this program;     *
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    *
 * <http://www.gnu.org/licenses/>.                                          *
 *                                                                          *
 ****************************************************************************/

#ifdef __APPLE__
.section __TEXT,__const
#else
.section .rodata
#endif
  .global _binary_config_kb_start
  .align 4
_binary_config_kb_start:
  .incbin "config.kb"
_binary_config_kb_end:
  .global _binary_config_kb_size
  .align 4
_binary_config_kb_size:
  .int  _binary_config_kb_end - _binary_config_kb_start
