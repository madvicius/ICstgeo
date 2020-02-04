timeFunction = function(y, #manter a entrada como matriz Y ou a funcao transforma em matriz?
                        t,
                        basetype = 'spline', # tipo de base, fohrier ou spline
                        nfunc = 3, # numero de altofuncoes
                        ...){ 
  
  require(fda) # tem como dependencia o pacote fda
  
  Y = matrix(y,ncol=length(t),byrow = TRUE) %>% t()
  
  baseFunction = switch (basetype,
                         'spline' = create.bspline.basis,
                         'fourier' = create.fourier.basis
  )
  
  bs = baseFunction(...) # baseFunction depende de qual foi definido 
  # no argumento basetype
  
  bsaux = predict(bs,t)
  bscoef = solve(crossprod(bsaux)+diag(ncol(bsaux))*10^(-20),
                 crossprod(bsaux,Y))
  
  fd = fd(bscoef,bs)
  fda = pca.fd(fd,nharm = 3)
  
  pcafd = fd(fda$harmonics$coefs,bs)
  
  eigenfunc = predict(pcafd,t)
  eigenval = fda$values[1:nfunc]
  
  out = list(
    'functions' = eigenfunc,
    'values' = eigenval
  )
  return(out)
  
}