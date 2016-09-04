(define-sdl-func int sdl-get-cpu-count () "SDL_GetCPUCount")
(define-sdl-func int sdl-get-cpu-cache-line-size () "SDL_GetCPUCacheLineSize")
(define-sdl-func sdl-bool-t sdl-has-rdtsc () "SDL_HasRDTSC")
(define-sdl-func sdl-bool-t sdl-has-alti-vec () "SDL_HasAltiVec")
(define-sdl-func sdl-bool-t sdl-has-mmx () "SDL_HasMMX")
(define-sdl-func sdl-bool-t sdl-has3-d-now () "SDL_Has3DNow")
(define-sdl-func sdl-bool-t sdl-has-sse () "SDL_HasSSE")
(define-sdl-func sdl-bool-t sdl-has-ss-e2 () "SDL_HasSSE2")
(define-sdl-func sdl-bool-t sdl-has-ss-e3 () "SDL_HasSSE3")
(define-sdl-func sdl-bool-t sdl-has-ss-e41 () "SDL_HasSSE41")
(define-sdl-func sdl-bool-t sdl-has-ss-e42 () "SDL_HasSSE42")
(define-sdl-func sdl-bool-t sdl-has-avx () "SDL_HasAVX")
(define-sdl-func sdl-bool-t sdl-has-av-x2 () "SDL_HasAVX2")
(define-sdl-func int sdl-get-system-ram () "SDL_GetSystemRAM")