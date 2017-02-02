------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--            Copyright (C) 2017, Free Software Foundation, Inc.            --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Calendar;
with Ada.Command_Line;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Directories;
with Ada.Text_IO;

with GNAT.MD5;
with GNAT.OS_Lib;
with GNAT.Sockets;

with GPR.Opt;

with GPR2.Compilation.Sync;
with GPR2.Containers;
with GPR2.Project.Pack;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Pack;

package body GPR2.Compilation.Slave is

   use Ada;

   -----------------
   -- Compute_Env --
   -----------------

   function Compute_Env
     (Tree : GPR2.Project.Tree.Object; Auto : Boolean) return String
   is
      use Ada.Command_Line;
      use GNAT;
      use GNAT.MD5;
      use GPR2;

      use type GNAT.OS_Lib.String_Access;
      use all type GPR2.Project.Registry.Attribute.Value_Kind;

      User      : OS_Lib.String_Access := OS_Lib.Getenv ("USER");
      User_Name : OS_Lib.String_Access := OS_Lib.Getenv ("USERNAME");
      Default   : constant String :=
                    (if User = null
                     then (if User_Name = null
                       then "unknown" else User_Name.all)
                     else User.all)
                    & '@' & GNAT.Sockets.Host_Name;

      package S_Set is new Ada.Containers.Indefinite_Ordered_Sets (String);

      Set : S_Set.Set;
      Ctx : Context;

   begin
      OS_Lib.Free (User);
      OS_Lib.Free (User_Name);

      if Auto then
         --  In this mode the slave environment is computed based on
         --  the project variable value and the command line arguments.

         --  First adds all command line arguments

         for K in 1 .. Argument_Count loop
            --  Skip arguments that are not changing the actual compilation and
            --  this will ensure that the same environment will be created for
            --  gprclean.

            if Argument (K) not in "-p" | "-d" | "-c" | "-q"
              and then
                (Argument (K)'Length < 2
                 or else Argument (K) (1 .. 2) /= "-j")
            then
               Set.Insert (Argument (K));
            end if;
         end loop;

         --  Then all the global variables for the project tree

         for Project of Tree loop
            if Project.Has_Variables then
               for V of Project.Variables loop
                  if V.Kind = Single then
                     Set.Include (String (V.Name) & "=" & String (V.Value));
                  end if;
               end loop;
            end if;
         end loop;

         --  Compute the MD5 sum of the sorted elements in the set

         for S of Set loop
            Update (Ctx, S);
         end loop;

         return Default & "-" & Digest (Ctx);

      else
         --  Otherwise use the default <user_name> & '@' & <host_name>
         return Default;
      end if;
   end Compute_Env;

   ----------------------------
   -- Register_Remote_Slaves --
   ----------------------------

   procedure Register_Remote_Slaves
     (Tree        : GPR2.Project.Tree.Object;
      Synchronize : Boolean)
   is
      use Ada.Directories;
      use GNAT.OS_Lib;

      use type GPR.Opt.Verbosity_Level_Type;
      use type Calendar.Time;
      use type Containers.Count_Type;

      Start, Stop : Calendar.Time;

      procedure Insert
        (List   : out Sync.Str_Vect.Vector;
         Values : GPR2.Containers.Value_List);
      --  Inserts all values into the vector

      Excluded_Patterns          : Sync.Str_Vect.Vector;
      Included_Patterns          : Sync.Str_Vect.Vector;
      Included_Artifact_Patterns : Sync.Str_Vect.Vector;

      ------------
      -- Insert --
      ------------

      procedure Insert
        (List   : out Sync.Str_Vect.Vector;
         Values : GPR2.Containers.Value_List) is
      begin
         for V of Values loop
            List.Append (V);
         end loop;
      end Insert;

      package Attrs renames GPR2.Project.Registry.Attribute;

      Project  : constant GPR2.Project.View.Object :=
                   Tree.Root_Project;

      Root_Dir : Unbounded_String renames GPR.Compilation.Slave.Root_Dir;

   begin
      Root_Dir := To_Unbounded_String (Remote_Root_Directory (Project));

      --  Check for Root_Dir attribute and Excluded_Patterns

      if Project.Has_Packages (GPR2.Project.Registry.Pack.Remote) then
         declare
            use GPR2.Project.Registry;

            Pck : constant GPR2.Project.Pack.Object :=
                    Project.Packages.Element
                      (GPR2.Project.Registry.Pack.Remote);
         begin
            if Pck.Has_Attributes (Attrs.Excluded_Patterns) then
               Insert
                 (Excluded_Patterns,
                  Pck.Attribute (Attrs.Excluded_Patterns).Values);

            elsif Pck.Has_Attributes (Attrs.Included_Patterns) then
               Insert
                 (Included_Patterns,
                  Pck.Attribute (Attrs.Included_Patterns).Values);

            elsif Pck.Has_Attributes (Attrs.Included_Artifacts_Patterns) then
               Insert
                 (Included_Artifact_Patterns,
                  Pck.Attribute (Attrs.Included_Artifacts_Patterns).Values);
            end if;
         end;
      end if;

      if not Exists (To_String (Root_Dir))
        or else not Is_Directory (To_String (Root_Dir))
      then
         Text_IO.Put_Line
           ("error: "
            & To_String (Root_Dir) & " is not a directory or does not exist");
         OS_Exit (1);

      else
         Text_IO.Put_Line ("root dir : " & To_String (Root_Dir));
      end if;

      --  Check if Excluded_Patterns and Included_Patterns are set

      if Included_Patterns.Length /= 0
        and then Excluded_Patterns.Length /= 0
      then
         Text_IO.Put_Line
           ("error: Excluded_Patterns and Included_Patterns are exclusive");
         OS_Exit (1);
      end if;

      --  Then registers the build slaves

      Start := Calendar.Clock;

      for S of GPR.Compilation.Slave.Slaves_Data loop
         GPR.Compilation.Slave.Register_Remote_Slave
           (S,
            String (Project.Name),
            Excluded_Patterns,
            Included_Patterns,
            Included_Artifact_Patterns,
            Synchronize);
      end loop;

      if Synchronize then
         Sync.Wait;
      end if;

      Stop := Calendar.Clock;

      if Synchronize and then GPR.Opt.Verbosity_Level > GPR.Opt.Low then
         Text_IO.Put ("  All data synchronized in ");
         Text_IO.Put (Duration'Image (Stop - Start));
         Text_IO.Put_Line (" seconds");
      end if;

      --  We are in remote mode, the initialization was successful, start tasks
      --  now.

      GPR.Compilation.Slave.Start_Waiting_Task;
   end Register_Remote_Slaves;

   ---------------------------
   -- Remote_Root_Directory --
   ---------------------------

   function Remote_Root_Directory
     (Project : GPR2.Project.View.Object) return String
   is
      use Ada.Directories;
      use GNAT.OS_Lib;

      package Attrs renames GPR2.Project.Registry.Attribute;

      Root_Dir : constant String :=
                   (Containing_Directory (GPR2.Value (Project.Path_Name)));
   begin
      if Project.Has_Packages (GPR2.Project.Registry.Pack.Remote) then
         declare
            use GPR2.Project.Registry;

            Pck : constant GPR2.Project.Pack.Object :=
                    Project.Packages.Element
                      (GPR2.Project.Registry.Pack.Remote);
         begin
            if Pck.Has_Attributes (Attrs.Root_Dir) then
               declare
                  RD : constant String :=
                         Pck.Attribute (Attrs.Root_Dir).Value;
               begin
                  if Is_Absolute_Path (RD) then
                     return RD;
                  else
                     return Normalize_Pathname
                       (Root_Dir & Directory_Separator & RD);
                  end if;
               end;
            end if;
         end;
      end if;

      return Root_Dir;
   end Remote_Root_Directory;

end GPR2.Compilation.Slave;
