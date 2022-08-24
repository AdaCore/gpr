--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  This package represents an entity source reference with an associated
--  message. This is mostly to report warnings/errors while parsing sources.

with GPR2.Source_Reference.Text_Value;

package GPR2.Source_Reference.Value is

   package Text_Values is new GPR2.Source_Reference.Text_Value (Value_Type);

   type Object is new Text_Values.Object with private;

   Undefined : constant Object;
   --  This constant is equal to any object declared without an explicit
   --  initializer.

   function Create
     (Filename     : Path_Name.Full_Name;
      Line, Column : Natural;
      Text         : Value_Type;
      At_Pos       : Unit_Index := No_Index) return Object'Class
     with Post => Create'Result.Is_Defined;

   function Create
     (Sloc         : GPR2.Source_Reference.Object;
      Text         : Value_Type;
      At_Pos       : Unit_Index := No_Index;
      From_Default : Boolean := False) return Object'Class
     with Post => Create'Result.Is_Defined;

   function Has_At_Pos (Self : Object) return Boolean
     with Pre => Self.Is_Defined;

   function At_Pos (Self : Object) return Unit_Index
     with Pre => Self.Is_Defined;

   function Is_From_Default (Self : Object) return Boolean
     with Pre => Self.Is_Defined;

private

   type Object is new Text_Values.Object with record
      At_Pos       : Unit_Index := No_Index;
      From_Default : Boolean := False;
      --  From_Default is only relevant for Target attribute value that may
      --  be evaluated as implicit one. The knowledge wether or not the target
      --  is default is necessary to decide i.e. on target fallback.
   end record;

   Undefined : constant Object := (Text_Values.Undefined with others => <>);

   function Create
     (Filename     : Path_Name.Full_Name;
      Line, Column : Natural;
      Text         : Value_Type;
      At_Pos       : Unit_Index := No_Index) return Object'Class
   is
     (Object'
        (Text_Values.Object
           (Text_Values.Create (Filename, Line, Column, Text)) with
            At_Pos => At_Pos, From_Default => False));

   function Create
     (Sloc         : GPR2.Source_Reference.Object;
      Text         : Value_Type;
      At_Pos       : Unit_Index := No_Index;
      From_Default : Boolean := False) return Object'Class
   is
     (Object'
        (Text_Values.Object
           (Text_Values.Create (Sloc, Text)) with
            At_Pos => At_Pos, From_Default => From_Default));

   function Has_At_Pos (Self : Object) return Boolean is
     (Self.At_Pos > 0);

   function At_Pos (Self : Object) return Unit_Index is
     (Self.At_Pos);

   function Is_From_Default (Self : Object) return Boolean is
     (Self.From_Default);

end GPR2.Source_Reference.Value;
