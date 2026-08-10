--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Strings.Fixed;

with GPR2.Project.Attribute;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Tree;
with GPR2.Tree_Internal;
with GPR2.View_Internal;

package body GPR2.Build.Actions.Process.Cargo_Support is

   --  Static 1-to-n mapping from GPR target name to compatible Rust triples.
   --  The first triple in each list is the default for automatic cross builds.
   --  This mapping and the Windows extra libs should eventually be moved to
   --  the knowledge base.

   type Triple_Array is array (1 .. 2) of Unbounded_String;

   type Mapping_Entry is record
      GPR           : Unbounded_String;
      Triples       : Triple_Array;
      Needs_Pthread : Boolean;
   end record;

   function US (S : String) return Unbounded_String
   renames To_Unbounded_String;

   Mapping : constant array (Positive range <>) of Mapping_Entry :=
     ((US ("aarch64-elf"),
       (1 => US ("aarch64-unknown-none"), 2 => <>),
       Needs_Pthread => False),
      (US ("aarch64-linux"),
       (1 => US ("aarch64-unknown-linux-gnu"), 2 => <>),
       Needs_Pthread => True),
      (US ("aarch64-qnx"),
       (1 => US ("aarch64-unknown-nto-qnx800"), 2 => <>),
       Needs_Pthread => False),
      (US ("aarch64-vx7r2"),
       (1 => US ("aarch64-wrs-vxworks-rtp"), 2 => <>),
       Needs_Pthread => False),
      (US ("arm-elf"),
       (1 => US ("armv7r-none-eabihf"), 2 => <>),
       Needs_Pthread => False),
      (US ("x86_64-linux"),
       (1 => US ("x86_64-unknown-linux-gnu"), 2 => <>),
       Needs_Pthread => True),
      (US ("x86_64-vx7r2"),
       (1 => US ("x86_64-wrs-vxworks-rtp"), 2 => <>),
       Needs_Pthread => False),
      (US ("x86_64-windows"),
       (1 => US ("x86_64-pc-windows-gnu"), 2 => <>),
       Needs_Pthread => False),
      (US ("x86_64-windows64"),
       (1 => US ("x86_64-pc-windows-gnu"), 2 => <>),
       Needs_Pthread => False));

   Windows_Extra_Libs :
     constant array (Positive range <>) of Unbounded_String :=
       (US ("-ladvapi32"),
        US ("-lbcrypt"),
        US ("-lgcc"),
        US ("-lgcc_eh"),
        US ("-lkernel32"),
        US ("-lmingw32"),
        US ("-lmingwex"),
        US ("-lmsvcrt"),
        US ("-lntdll"),
        US ("-luser32"),
        US ("-luserenv"),
        US ("-lws2_32"));

   function Default_Triple (GPR_Target : Name_Type) return String;
   --  The first (default) Rust triple mapped from GPR_Target, or the empty
   --  string when GPR_Target has no known mapping.

   --------------------
   -- Default_Triple --
   --------------------

   function Default_Triple (GPR_Target : Name_Type) return String is
   begin
      for M of Mapping loop
         if M.GPR = String (GPR_Target) then
            return To_String (M.Triples (1));
         end if;
      end loop;
      return "";
   end Default_Triple;

   ------------
   -- Driver --
   ------------

   function Driver (View : GPR2.Project.View.Object) return String is
      package PRA renames GPR2.Project.Registry.Attribute;
      package PAI renames GPR2.Project.Attribute_Index;

      Driver_Attr : constant GPR2.Project.Attribute.Object :=
        View.Attribute (PRA.Compiler.Driver, PAI.Create (Rust_Language));
      Tree_Int    : constant access GPR2.Tree_Internal.Object :=
        GPR2.View_Internal.Get_RO (View).Tree;
   begin
      if Tree_Int.Languages_To_Compilers.Contains (Rust_Language) then
         return Tree_Int.Languages_To_Compilers.Element (Rust_Language);
      elsif Driver_Attr.Is_Defined then
         return Driver_Attr.Value.Text;
      else
         return "";
      end if;
   end Driver;

   ------------------------
   -- Extra_Link_Options --
   ------------------------

   function Extra_Link_Options
     (Triple : String) return GPR2.Containers.Value_List
   is
      Result : GPR2.Containers.Value_List;
   begin
      for M of Mapping loop
         for T of M.Triples loop
            if To_String (T) = Triple then
               if M.Needs_Pthread then
                  Result.Append ("-pthread");
               end if;

               if Ada.Strings.Fixed.Index (Triple, "windows") > 0 then
                  for Lib of Windows_Extra_Libs loop
                     Result.Append (To_String (Lib));
                  end loop;
               end if;

               return Result;
            end if;
         end loop;
      end loop;

      return Result;
   end Extra_Link_Options;

   -----------
   -- Image --
   -----------

   function Image (Kind : Cargo_Lib_Kind) return String is
   begin
      case Kind is
         when Static_Library => return "staticlib";
         when Shared_Library => return "cdylib";
         when Unsupported    => return "<unsupported>";
         when Ambiguous      => return "<ambiguous>";
      end case;
   end Image;

   -------------------
   -- Is_Compatible --
   -------------------

   function Is_Compatible
     (GPR_Target : Name_Type; Triple : String) return Boolean is
   begin
      for M of Mapping loop
         if M.GPR = String (GPR_Target) then
            for T of M.Triples loop
               if To_String (T) = Triple then
                  return True;
               end if;
            end loop;
            return False;
         end if;
      end loop;
      return False;
   end Is_Compatible;

   -----------------------
   -- Library_File_Name --
   -----------------------

   function Library_File_Name
     (View : GPR2.Project.View.Object;
      Kind : Cargo_Lib_Kind) return Simple_Name
   is
      package PRA renames GPR2.Project.Registry.Attribute;

      Tree_Int  : constant access GPR2.Tree_Internal.Object :=
        GPR2.View_Internal.Get_RO (View).Tree;
      File_Name : Unbounded_String;

   begin
      case Kind is
         when Static_Library =>
            Append
              (File_Name, View.Attribute (PRA.Archive_Prefix).Value.Text);
            Append (File_Name, String (View.Library_Name));
            Append (File_Name, String (Tree_Int.Archive_Suffix));

         when Shared_Library =>
            Append
              (File_Name,
               View.Attribute (PRA.Shared_Library_Prefix).Value.Text);
            Append (File_Name, String (View.Library_Name));
            Append
              (File_Name,
               View.Attribute (PRA.Shared_Library_Suffix).Value.Text);

         when Unsupported | Ambiguous =>
            --  Ruled out by the precondition
            raise Internal_Error with "unsupported crate kind";
      end case;

      return Simple_Name (To_String (File_Name));
   end Library_File_Name;

   --------------
   -- Manifest --
   --------------

   function Manifest
     (View : GPR2.Project.View.Object) return GPR2.Path_Name.Object
   is (Root_Directory (View).Compose ("Cargo.toml"));

   --------------------
   -- Root_Directory --
   --------------------

   function Root_Directory
     (View : GPR2.Project.View.Object) return GPR2.Path_Name.Object
   is
      package PRA renames GPR2.Project.Registry.Attribute;

      Root : constant GPR2.Project.Attribute.Object :=
        View.Attribute (PRA.Cargo.Root);
   begin
      --  Cargo.Root defaults to the project directory, so a view outside an
      --  aggregate always has one

      pragma Assert (Root.Is_Defined);

      return GPR2.Path_Name.Create_Directory
        (Filename_Type (Root.Value.Text), View.Dir_Name.Value);
   end Root_Directory;

   -----------------
   -- Rust_Triple --
   -----------------

   function Rust_Triple (View : GPR2.Project.View.Object) return String is
      package PRA renames GPR2.Project.Registry.Attribute;

      Rust_Target_Attr : constant GPR2.Project.Attribute.Object :=
        View.Attribute (PRA.Cargo.Rust_Target);
   begin
      if Rust_Target_Attr.Is_Defined then
         return Rust_Target_Attr.Value.Text;
      else
         return Default_Triple (View.Tree.Target);
      end if;
   end Rust_Triple;

   -----------------------
   -- To_Cargo_Lib_Kind --
   -----------------------

   function To_Cargo_Lib_Kind
     (Cargo_Lib_Types : GPR2.Containers.Value_List) return Cargo_Lib_Kind
   is
      Found : Cargo_Lib_Kind := Unsupported;
   begin
      for Cargo_Lib_Type of Cargo_Lib_Types loop
         if Cargo_Lib_Type = "staticlib" then
            if Found /= Unsupported then
               return Ambiguous;
            end if;

            Found := Static_Library;

         elsif Cargo_Lib_Type = "cdylib" then
            if Found /= Unsupported then
               return Ambiguous;
            end if;

            Found := Shared_Library;
         end if;
      end loop;

      return Found;
   end To_Cargo_Lib_Kind;

end GPR2.Build.Actions.Process.Cargo_Support;
