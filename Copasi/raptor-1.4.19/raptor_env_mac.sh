#please also remember to keep libxml2 undefined in raptor_config.h
#for raptor, have to build i386 and x86_64 separately then use lipo to create 
#universal lib.
#lipo -create -arch x86_64 libraptor_64.a -arch i386 libraptor_i386.a -output libraptor.a

#export CFLAGS='-fPIC -arch i386 -mmacosx-version-min=10.6 -isysroot /Developer/SDKs/MacOSX10.6.sdk'
#export CXXFLAGS='-fPIC -arch i386 -mmacosx-version-min=10.6 -isysroot /Developer/SDKs/MacOSX10.6.sdk'
#export LDFLAGS='-fPIC -arch i386 -isysroot /Developer/SDKs/MacOSX10.6.sdk -mmacosx-version-min=10.6 -no_compact_linkedit'

export CFLAGS='-fPIC -arch x86_64 -mmacosx-version-min=10.6 -isysroot /Developer/SDKs/MacOSX10.6.sdk'
export CXXFLAGS='-fPIC -arch x86_64 -mmacosx-version-min=10.6 -isysroot /Developer/SDKs/MacOSX10.6.sdk'
export LDFLAGS='-fPIC -arch x86_64 -isysroot /Developer/SDKs/MacOSX10.6.sdk -mmacosx-version-min=10.6 -no_compact_linkedit'
