--  No need to explicitly flag grandchild package, since it cannot exist when
--  you remove the child one

package Foo.Child.Grandchild is -- NOFLAG
end Foo.Child.Grandchild;
