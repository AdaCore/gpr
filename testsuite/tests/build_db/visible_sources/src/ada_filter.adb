with GPR2.Build.Source.Sets;
with GPR2.Build.Source_Base;
with GPR2.Project.View;

function Ada_Filter
  (View   : GPR2.Project.View.Object;
   Source : GPR2.Build.Source_Base.Object'Class;
   Data   : GPR2.Build.Source.Sets.Filter_Data'Class) return Boolean
is
   pragma Unreferenced (View, Data);
   use type GPR2.Language_Id;
begin
   return Source.Language = GPR2.Ada_Language;
end Ada_Filter;
