------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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

--  This package handles the creation, manipulation of View_Id objects.
--  View_Id(s) are used to identify uniquely views associated with project
--  files (see GPR2.Project.View).

with Ada.Containers;

with GPR2.Path_Name;
with GPR2.Context;

package GPR2.View_Ids is

   View_Id_Error : exception;
   --  Exception raised whenever preconditions are not met

   type View_Id is private;
   --  Represents an ID for a given project view

   Undefined    : constant View_Id;
   --  Null id

   Config_View_Id  : constant View_Id;
   --  Id for the view associated with the Configuration project

   Runtime_View_Id : constant View_Id;
   --  Id for the view associated with the Runtime project

   Root      : constant GPR2.Context.Context_Kind := GPR2.Context.Root;
   Aggregate : constant GPR2.Context.Context_Kind := GPR2.Context.Aggregate;

   function Create
     (Project_File : GPR2.Path_Name.Object;
      Context      : GPR2.Context.Context_Kind := Root)
      return View_Id
     with Pre  => Project_File.Is_Defined
                    and then Project_File.Has_Dir_Name,
         Post => Is_Defined (Create'Result);
   --  Creates an Id for the view associated with Project_File either using
   --  the root aggregate project context or the root context. Note that
   --  Project_File should point to an absolute location (i.e
   --  Project_File.Has_Dir_Name should be True).

   function Hash (Self : View_Id) return Ada.Containers.Hash_Type
     with Pre => Is_Defined (Self);
   --  Computes the hash value of a view id. The hash value of a null view id
   --  is not allowed. The reason is that the Hash function is mainly used
   --  when using containers such as Maps or Sets.

   function Image (Self : View_Id) return Optional_Name_Type
     with Pre => Is_Defined (Self);
   --  Returns a string image of a view id. The resulting string can be used
   --  to rebuild the View_Id object (see Import).

   function Import (Name : Optional_Name_Type) return View_Id
     with Pre => Is_Valid_Image (Name);
   --  Returns the View_Id object V for which Image (V) = Str. View_Id_Error
   --  is raised in case Str is not a valid view id image.

   function Is_Defined (Id : View_Id) return Boolean;
   --  Returns False if the Id is Null_Id.

   function Is_Valid_Image (Name : Optional_Name_Type) return Boolean;
   --  Returns True if Str may correspond to a View_Id image.

   overriding function "=" (Self : View_Id; Other : View_Id) return Boolean;
   --  Return True if Self = Other

   function "<" (Self : View_Id; Other : View_Id) return Boolean;
   --  Return True if Image (Self) < Image (Other). This is used only to
   --  instantiate containers such as ordered sets or maps.

private

   type View_Id_Kind is
      (Null_Id,
       Config_Id,
       Runtime_Id,
       Project_Id);
   --  The distinct View_Id kinds (null, configuration project, runtime project
   --  and view corresponding to a project file).

   type View_Id (Kind : View_Id_Kind := Null_Id) is record
      case Kind is
         when Null_Id    => null;
         when Config_Id  => null;
         when Runtime_Id => null;
         when Project_Id =>
            Context : GPR2.Context.Context_Kind;
            Id      : Unbounded_String;
      end case;
   end record;

   Undefined       : constant View_Id := (Kind => Null_Id);
   Config_View_Id  : constant View_Id := (Kind => Config_Id);
   Runtime_View_Id : constant View_Id := (Kind => Runtime_Id);

   function Is_Defined (Id : View_Id) return Boolean
     is (Id /= Null_View_Id);

end GPR2.View_Ids;
