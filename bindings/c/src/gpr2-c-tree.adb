--
--  Copyright (C) 2020-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Unchecked_Deallocation;

with GNATCOLL.JSON;

with GPR2.Log;
with GPR2.Project.Tree;

with GPR2.C.JSON;
with GPR2.C.JSON.Encoders; use GPR2.C.JSON.Encoders;
with GPR2.C.Utils;         use GPR2.C.Utils;

package body GPR2.C.Tree is

   use GPR2.C.JSON;

   ----------------------------
   -- Invalidate_Source_List --
   ----------------------------

   procedure Invalidate_Source_List
      (Request : JSON_Value; Result : JSON_Value)
   is
      pragma Unreferenced (Result);

      Tree : constant GPR_Tree_Access := Get_GPR_Tree (Request, "tree_id");
   begin
      GPR2.Project.Tree.Invalidate_Sources (Self => Tree.all);
   end Invalidate_Source_List;

   ---------
   -- Load--
   ---------

   procedure Load (Request : JSON_Value; Result : JSON_Value) is
      Tree : constant GPR_Tree_Access := new GPR2.Project.Tree.Object;
   begin
      Load_Project
        (Tree             => Tree.all,
         Filename         => To_String (Get (Request, "filename"), ""),
         Context          => To_Context (Get (Request, "context")),
         Build_Path       => To_String (Get (Request, "build_path"), ""),
         Subdirs          => To_String (Get (Request, "subdirs"), ""),
         Src_Subdirs      => To_String (Get (Request, "src_subdirs"), ""),
         Project_Dir      => To_String (Get (Request, "project_dir"), ""),
         Check_Shared_Lib =>
           To_Boolean (Get (Request, "check_shared_lib"), True),
         Absent_Dir_Error =>
           To_Boolean (Get (Request, "absent_dir_error"), False),
         Implicit_With => To_Path_Name_Set (Get (Request, "implicit_with")),
         Config        => To_String (Get (Request, "config"), ""),
         Target        => To_String (Get (Request, "target"), ""),
         Runtimes      => To_Lang_Value_Map (Get (Request, "runtimes")));

      Set (Result, "id", From_GPR_Tree (Tree));

      if Tree.Is_Defined then
         Set (Result, "root_view", From_GPR_View (Root_View (Tree.all)));
         Set
           (Result, "runtime_view",
            From_GPR_View (Runtime_View (Tree.all)));
         Set
           (Result, "target",
            From_Name (Tree.all.Target (Canonical => False)));
         Set
           (Result, "canonical_target",
            From_Name (Tree.all.Target (Canonical => True)));
         Set
           (Result, "search_paths",
            From_GPR_Paths (Search_Paths (Tree.all)));
         if GPR2.Project.Tree.Has_Src_Subdirs (Tree.all) then
            Set
              (Result, "src_subdirs",
               From_Filename (GPR2.Project.Tree.Src_Subdirs (Tree.all)));
         else
            Set (Result, "src_subdirs", GNATCOLL.JSON.JSON_Null);
         end if;
         Set
           (Result, "subdirs",
            From_Filename (GPR2.Project.Tree.Subdirs (Tree.all)));
         Set
           (Result, "build_path",
            From_GPR_Path (GPR2.Project.Tree.Build_Path (Tree.all)));
         Set (Result, "views", From_GPR_Views (Tree.all.Ordered_Views));

         if Tree.Has_Context then
            Set (Result, "context", From_Context (Tree.all.Context));
         else
            Set (Result, "context", GNATCOLL.JSON.Create_Object);
         end if;
      end if;
   end Load;

   ------------------
   -- Log_Messages --
   ------------------

   procedure Log_Messages (Request : JSON_Value; Result : JSON_Value) is
      Tree : constant GPR_Tree_Access := Get_GPR_Tree (Request, "tree_id");
      Information : constant Boolean         :=
        To_Boolean (Get (Request, "information"), True);
      Warning : constant Boolean :=
        To_Boolean (Get (Request, "warning"), True);
      Error : constant Boolean := To_Boolean (Get (Request, "error"), True);
      Read   : constant Boolean := To_Boolean (Get (Request, "read"), True);
      Unread : constant Boolean :=
        To_Boolean (Get (Request, "unread"), True);

      Message_Array : GNATCOLL.JSON.JSON_Array;
      Messages      : constant GNATCOLL.JSON.JSON_Value :=
        GNATCOLL.JSON.Create (Message_Array);
   begin
      if Tree.all.Has_Messages then
         for C in Tree.all.Log_Messages.Iterate
           (Information => Information, Warning => Warning, Error => Error,
            Read        => Read, Unread => Unread)
         loop
            Messages.Append (From_GPR_Message (GPR2.Log.Element (C)));
         end loop;
      end if;

      GNATCOLL.JSON.Set_Field (Result, "messages", Messages);
   end Log_Messages;

   ------------
   -- Unload --
   ------------

   procedure Unload (Request : JSON_Value; Result : JSON_Value) is
      pragma Unreferenced (Result);

      procedure Free is new Ada.Unchecked_Deallocation
        (GPR2.Project.Tree.Object, GPR_Tree_Access);

      Tree : GPR_Tree_Access := Get_GPR_Tree (Request, "tree_id");
   begin
      GPR2.Project.Tree.Unload (Tree.all);
      Free (Tree);
   end Unload;

   -------------------------
   -- Update_Source_Infos --
   -------------------------

   procedure Update_Source_Infos (Request : JSON_Value; Result : JSON_Value) is
      pragma Unreferenced (Result);
   begin
      Update_Source_Infos
        (Tree                 => Get_GPR_Tree (Request, "tree_id").all,
         Allow_Source_Parsing =>
           To_Boolean (Get (Request, "allow_source_parsing"), False));
   end Update_Source_Infos;

   ------------------------
   -- Update_Source_List --
   ------------------------

   procedure Update_Source_List (Request : JSON_Value; Result : JSON_Value) is
      pragma Unreferenced (Result);
   begin
      Update_Source_List (Tree => Get_GPR_Tree (Request, "tree_id").all);
   end Update_Source_List;

end GPR2.C.Tree;
