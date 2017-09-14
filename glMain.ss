(library (glMain)
  (export glRender)
  (import (chezscheme) (glut) (glu) (gl))

  (define glRender
    (lambda (loader renderer)
      (begin 
	(glutInit 0 0)

	(glutInitDisplayMode (bitwise-ior GLUT_DEPTH GLUT_RGBA GLUT_DOUBLE))

	(glutInitWindowSize 480 854)

	(glutCreateWindow "gl")

	(glutDisplayFunc renderer)

	(loader)

	(glutMainLoop)

	))))
