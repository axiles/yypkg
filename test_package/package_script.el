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
  ("prefix" (MKdir "${PREFIX}/prefix"))
  ("prefix" (Expand "a" "${PREFIX}/prefix"))
  ("prefix" (Expand "x/y" "${PREFIX}/prefix"))
  ("t" (MKdir "${PREFIX}/prefix/t"))
  ("t" (Expand "x/z/omega" "${PREFIX}/prefix/t"))
 )
 (
  (Reverse "t")
  (Reverse "prefix") 
 )
)
