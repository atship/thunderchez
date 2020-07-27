(library (mt19937)
         (export 
           mt-seed
           mt-rand)
         (import (chezscheme))
;; compile the mt19937.cpp to mt19937.dylib/mt19937.so
;; you can do it with: g++ -dynamiclib -o /usr/local/lib/mt19937.dylib mt19937.cpp or g++ -shared -fPIC -o /usr/local/lib/mt19937.so mt19937.cpp

         (define mt-init
           (load-shared-object "mt19937.dylib")
           )

         (define mt-seed
           (foreign-procedure "mt19937_seed" (int) void))

         (define mt-rand
           (foreign-procedure "mt19937" (int) int)))
