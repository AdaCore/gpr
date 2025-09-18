with Ada.Strings.Unbounded;

package Pkg is

   function Get return Ada.Strings.Unbounded.Unbounded_String;

private

   function Get return Ada.Strings.Unbounded.Unbounded_String is
     (Ada.Strings.Unbounded.Null_Unbounded_String);

end Pkg;