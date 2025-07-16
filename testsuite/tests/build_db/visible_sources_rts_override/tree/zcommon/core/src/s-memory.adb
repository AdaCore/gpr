with GNATCOLL.Memory;

package body System.Memory is
   package M renames GNATCOLL.Memory;

   function Alloc (Size : size_t) return System.Address is
   begin
      return M.Alloc (M.size_t (Size));
   end Alloc;

   procedure Free (Ptr : System.Address) renames M.Free;

   function Realloc
     (Ptr  : System.Address;
      Size : size_t)
      return System.Address is
   begin
      return M.Realloc (Ptr, M.size_t (Size));
   end Realloc;

end System.Memory;
