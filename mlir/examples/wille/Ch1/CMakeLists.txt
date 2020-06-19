# For a better template to copy, see examples/standalone
set(LLVM_LINK_COMPONENTS
  Support
  )

add_toy_chapter(willec-ch1
  willec.cpp
  parser/AST.cpp
  )
include_directories(include/)
target_link_libraries(willec-ch1
  PRIVATE
    MLIRSupport)
