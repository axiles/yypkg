(
 (
  (credits "me")
  (package_name "test")
  (package_size_expanded "1MB")
  (package_type Other)
  (package_version (Beta 0 0 0 0))
  (packager_email "a@a.com")
  (packager_name "ME")
 )
 (
  ("prefix" (Expand "a" "."))
  ("prefix" (Expand "x/y" "."))
  ("t" (Expand "x/z/omega" "t"))
 )
 (
  (Reverse "t")
  (Reverse "prefix") 
 )
)
