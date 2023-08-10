package First_Package is
   procedure Call;
end First_Package;

--  Multi-units is not supported. The parsing shall not reach
--  this part.
package Second_Package is
   procedure Call;
end Second_Package;