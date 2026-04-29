package FOO.Config_Pkg is

   pragma Pure;

   Version : constant String := "FOO";

   Unix_Separator    : constant Character := '/';
   Windows_Separator : constant Character := '\';

   Separator    : constant Character := Windows_Separator;

end FOO.Config_Pkg;
