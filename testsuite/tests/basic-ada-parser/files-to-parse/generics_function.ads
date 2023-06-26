with Toto;
with A.Titi;
with A.Titi.Tata;

generic
   type Element_Type is private;
   --  The elements that are pooled

   type Factory_Param is private;
   with function Factory (Param : Factory_Param) return Element_Type;
   --  Information needed to create new elements as needed. This is passed as
   --  is to the Factory function.

   type Resource_Set is (<>);
   --  Represents a set of elements within the pool.
   --  There can be multiple such sets in one pool. Each set is associated with
   --  its own factory parameter, but all other elements are compatible between
   --  the various sets (in particular, the resources are the same, so the rest
   --  of the application doesn't need to know which set this resource is
   --  from).
   --  Most times, a pool will need only one such subset, which is created by
   --  default. All subprograms below apply to this default set, unless
   --  otherwise specified.

   with procedure Free (Self : in out Element_Type) is null;
   --  Called when the [Self] is finally removed from the pool

   with procedure On_Release (Self : in out Element_Type) is null;
   --  Called when Self is released into the pool.
   --  The application has no more reference to that element, apart from the
   --  one in the pool.
   --  The result of Element.Element should not be freed yet, since it is
   --  returned to the pool (instead, override the formal [Free] parameter).
   --  But any other custom field from Element should be reset at that time.

   with procedure Free_Param (Data : in out Factory_Param) is null;
   --  Free Factory_Param.
   --  Called when the pool itself is freed.

function My_Generic_Function (Toto: Integer ; Tata : Integer) return Boolean;