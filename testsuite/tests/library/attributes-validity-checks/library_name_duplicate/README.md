
A ----> B means A imports B

# Hierarchy of projects

Lib1 ----> Lib1_Clash

Lib2_Clash1 ----> Lib2 <----------\
                   ^              |
                   |              |
                   |              |
Main1 ----> Lib2_clash2 ----> Lib2_clash3
               ^
               |
Main2  --------/

And aggregate.gpr aggregates Lib1, Lib2_Clash1, Main1 and Main2.
