------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
--                                                                          --
-- This is  free  software;  you can redistribute it and/or modify it under --
-- terms of the  GNU  General Public License as published by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details.  You should have received  a copy of the  GNU  --
-- General Public License distributed with GNAT; see file  COPYING. If not, --
-- see <http://www.gnu.org/licenses/>.                                      --
--                                                                          --
------------------------------------------------------------------------------

with GPR2.C.JSON; use GPR2.C.JSON;
with GPR2.Project.View;
with GPR2.Project.Attribute;

package body GPR2.C is

   type Bind_Handler is access procedure
      (Request : JSON_Value; Result : JSON_Value);

   function Bind
      (Request : C_Request;
       Answer  : out C_Answer;
       Handler : Bind_Handler) return C_Status;
   --  Takes care of the bolierplate that decodes incoming request and encode
   --  answer. Handler function receives the decoded JSON and returns the
   --  'result' part of the answer.
   --  Bind catches all exceptions and sets properly the error in the answer.
   --  Note that Result is in fact an in/out paramter. When the handler is
   --  called, the Result JSON_Value is initialized as an empty JSON object
   --  (i.e: dictionary).

   ----------
   -- Bind --
   ----------

   function Bind
      (Request : C_Request;
       Answer  : out C_Answer;
       Handler : Bind_Handler) return C_Status
   is
      Request_Obj : JSON.JSON_Value;
      Answer_Obj  : constant JSON_Value := Initialize_Answer;
      Result_Obj : JSON_Value;
   begin

      Result_Obj := Get_Result (Answer_Obj);

      --  Until this stage error can occurs only if there is a lack of
      --  memory in which case nothing can really be done.
      begin
         Request_Obj := Decode (Request);
      exception
         when E : others =>
            --  Error detected during parsing of the request JSON.
            Set_Status (Answer_Obj, Invalid_Request, E);
      end;

      begin
         Handler (Request => Request_Obj,
                  Result  => Result_Obj);
      exception
         when E : others =>
            Set_Status (Answer_Obj, Call_Error, E);
      end;

      --  Unless there is a bug in the GNATCOLL.JSON library, all relevant
      --  errors have been catched. No exception is expected from here.
      Answer := Encode (Answer_Obj);
      return Get_Status (Answer_Obj);
   end Bind;

   --------------------------------
   -- GPR2_Project_Tree_Get_View --
   --------------------------------

   function GPR2_Project_Tree_Get_View
      (Request : C_Request;
       Answer  : out C_Answer) return C_Status
   is
      procedure Handler (Request : JSON_Value; Result : JSON_Value);

      -------------
      -- Handler --
      -------------

      procedure Handler (Request : JSON_Value; Result : JSON_Value)
      is
         Tree : Project_Tree_Access;
         View : constant Project_View_Access := new GPR2.Project.View.Object;
      begin
         Tree := Get_Project_Tree (Request, "project_id");
         View.all := GPR2.Project.Tree.Get_View (
            Tree.all,
            GPR2.Name_Type (Get_String (Request, "unit")));
         Set_Project_View (Result, "view_id", View);
      end Handler;

   begin
      return Bind (Request, Answer, Handler'Unrestricted_Access);
   end GPR2_Project_Tree_Get_View;

   --------------------------------
   -- GPR2_Project_Load_Autoconf --
   --------------------------------

   function GPR2_Project_Tree_Load_Autoconf
      (Request : C_Request;
       Answer  : out C_Answer) return C_Status
   is
      procedure Handler (Request : JSON_Value; Result : JSON_Value);

      procedure Handler (Request : JSON_Value; Result : JSON_Value)
      is
         Tree : constant Project_Tree_Access := new GPR2.Project.Tree.Object;
      begin
         GPR2.Project.Tree.Load_Autoconf
            (Self        => Tree.all,
             Filename    => Get_File_Path (Request, "filename"),
             Context     => Get_Context (Request, "context"),
             Build_Path  => Get_Optional_Dir_Path (Request, "build_path"),
             Subdirs     =>
               Optional_Name_Type (Get_String (Request, "subdirs", "")),
             Src_Subdirs =>
               Optional_Name_Type (Get_String (Request, "src_subdirs", "")),
             Check_Shared_Lib =>
               Get_Boolean (Request, "check_shared_lib", True),
             Implicit_Project =>
               Get_Boolean (Request, "implicit_project", False),
             Absent_Dir_Error =>
               Get_Boolean (Request, "absent_dir_error", False));

         Set_Project_Tree (Result, "project_id", Tree);
      end Handler;

   begin

      return Bind (Request, Answer, Handler'Unrestricted_Access);
   end GPR2_Project_Tree_Load_Autoconf;

   ------------------------------------
   -- GPR2_Project_Tree_Root_Project --
   ------------------------------------

   function GPR2_Project_Tree_Root_Project
      (Request : C_Request;
       Answer  : out C_Answer)
      return C_Status
   is
      procedure Handler (Request : JSON_Value; Result : JSON_Value);

      procedure Handler (Request : JSON_Value; Result : JSON_Value)
      is
         Tree : Project_Tree_Access;
         View : constant Project_View_Access := new GPR2.Project.View.Object;
      begin
         Tree := Get_Project_Tree (Request, "project_id");
         View.all := GPR2.Project.Tree.Root_Project (Tree.all);
         Set_Project_View (Result, "view_id", View);
      end Handler;
   begin
      return Bind (Request, Answer, Handler'Unrestricted_Access);
   end GPR2_Project_Tree_Root_Project;

   ---------------------------------
   -- GPR2_Project_View_Attribute --
   ---------------------------------

   function GPR2_Project_View_Attribute
      (Request : C_Request;
       Answer  : out C_Answer)
      return C_Status
   is
      procedure Handler (Request : JSON_Value; Result : JSON_Value);

      procedure Handler (Request : JSON_Value; Result : JSON_Value)
      is
         View : constant Project_View_Access :=
            Get_Project_View (Request, "view_id");
         Attr : GPR2.Project.Attribute.Object;
      begin
         Attr := GPR2.Project.View.Attribute
            (Self => View.all,
             Name => Name_Type (Get_String (Request, "name")),
             Index => Value_Type (Get_String (Request, "index", "")));
         Set_Project_Attribute (Result, "attr", Attr);
      end Handler;
   begin
      return Bind (Request, Answer, Handler'Unrestricted_Access);
   end GPR2_Project_View_Attribute;
end GPR2.C;
