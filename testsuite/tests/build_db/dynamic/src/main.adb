with Scenario;

procedure Main is

begin
   Scenario.Execute ("scenario/simple.txt");
   Scenario.Execute ("scenario/extending.txt");
   Scenario.Execute ("scenario/agglib.txt");
end Main;
