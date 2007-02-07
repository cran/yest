library(MASS)

gen1<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {
    a<-0;
    b<-runif(1,-0.9,0.9);
    c<-runif(1,-0.9,0.9);
    d<-runif(1,-0.9,0.9);
    e<-runif(1,-0.9,0.9);
    f<-runif(1,-0.9,0.9);

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (inv) return(v) else return(solve(v))
}

gen2<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {
    b<-runif(1,-0.9,0.9);
    c<-runif(1,-0.9,0.9);
    d<-runif(1,-0.9,0.9);
    e<-runif(1,-0.9,0.9);
    f<-runif(1,-0.9,0.9);
    a<-c*e;

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (inv) return(v) else return(solve(v))
}

gen3<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {
    b<-runif(1,-0.9,0.9);
    c<-runif(1,-0.9,0.9);
    d<-runif(1,-0.9,0.9);
    e<-runif(1,-0.9,0.9);
    f<-runif(1,-0.9,0.9);
    a<-(c*e+b*d-f*(b*e+c*d))/(1-f^2);

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (inv) return(v) else return(solve(v))
}

gen4<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {
    a<-0;
    c<-runif(1,-0.9,0.9);
    d<-runif(1,-0.9,0.9);
    e<-runif(1,-0.9,0.9);
    f<-runif(1,-0.9,0.9);
    b<- -c*(d*f-e)/(e*f-d);

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (inv) return(v) else return(solve(v))
}

gen5<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {
    a<-0;
    c<-runif(1,-0.9,0.9);
    d<-runif(1,-0.9,0.9);
    e<-runif(1,-0.9,0.9);
    b<-runif(1,-0.9,0.9);
    f<-(d*(1-c^2)+b*e*c)/e;

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (inv) return(v) else return(solve(v))
}

gen6<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {
    a<-0;
    c<-runif(1,-0.9,0.9);
    d<-runif(1,-0.9,0.9);
    e<-runif(1,-0.9,0.9);
    b<-runif(1,-0.9,0.9);
    f<-b*c+d*e;

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (inv) return(v) else return(solve(v))
}

gen7<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {
    a<-0;
    c<-runif(1,-0.9,0.9);
    d<-runif(1,-0.9,0.9);
    e<-runif(1,-0.9,0.9);
    b<-runif(1,-0.9,0.9);
    f<-b*c;

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (inv) return(v) else return(solve(v))
}

gen8<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {
    
    c<-runif(1,-0.9,0.9);
    d<-runif(1,-0.9,0.9);
    e<-runif(1,-0.9,0.9);
    b<-runif(1,-0.9,0.9);
    f<-b*c;
    a<-c*e;

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (inv) return(v) else return(solve(v))
}

gen9<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {
    
    c<-runif(1,-0.9,0.9);
    b<-runif(1,-0.9,0.9);
    e<-runif(1,-0.9,0.9);
    d<-runif(1,-0.9,0.9);
    f<-0;
    a<-0;

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (!inv) return(v) else return(solve(v))
}

gen10<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {
    
    c<-runif(1,-0.9,0.9);
    d<-runif(1,-0.9,0.9);
    e<-runif(1,-0.9,0.9);
    f<-runif(1,-0.9,0.9);
    b<-(f+d*e*c^2-d*e-c^2*e^2*f)/(c*(1-e^2));
    a<-c*e;

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (inv) return(v) else return(solve(v))
}

gen11<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {
    
    c<-runif(1,-0.9,0.9);
    b<-runif(1,-0.9,0.9);
    e<-runif(1,-0.9,0.9);
    f<-runif(1,-0.9,0.9);
    d<-e*f;
    a<-0;

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (inv) return(v) else return(solve(v))
}

gen12<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {
    
    c<-runif(1,-0.9,0.9);
    b<-runif(1,-0.9,0.9);
    e<-runif(1,-0.9,0.9);
    d<-runif(1,-0.9,0.9);
    f<-0;
    a<-0;

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (inv) return(v) else return(solve(v))
}

gen13<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {
    
    c<-runif(1,-0.9,0.9);
    f<-runif(1,-0.9,0.9);
    e<-runif(1,-0.9,0.9);
    d<-runif(1,-0.9,0.9);
    a<-c*e;  
    b<-c*e/d;

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (inv) return(v) else return(solve(v))
}

gen14<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {
    
    c<-runif(1,-0.9,0.9);
    f<-runif(1,-0.9,0.9);
    e<-runif(1,-0.9,0.9);
    d<-runif(1,-0.9,0.9);
    a<-c*e;  
    b<-(c*(1-d^2-e^2+e*d*f)/(f-d*e));

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (inv) return(v) else return(solve(v))
}

gen15<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {
    
    b<-runif(1,-0.9,0.9);
    f<-runif(1,-0.9,0.9);
    e<-runif(1,-0.9,0.9);
    d<-runif(1,-0.9,0.9);
    c<-b*f;  
    a<-e*b*f;

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (inv) return(v) else return(solve(v))
}


gen16<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {
    
    b<-runif(1,-0.9,0.9);
    e<-runif(1,-0.9,0.9);
    f<-runif(1,-0.9,0.9);
    c<-0;
    d<-0;
    a<- -b*e*f/(1-f^2);

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (!inv) return(v) else return(solve(v))
}

gen17<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {
    
    b<-runif(1,-0.9,0.9);
    f<-runif(1,-0.9,0.9);
    e<-runif(1,-0.9,0.9);
    c<-b*f; 
    a<-0; 
    d<-f*(1-b^2)/e;

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (inv) return(v) else return(solve(v))
}

gen18<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {
    
    b<-runif(1,-0.9,0.9);
    c<-runif(1,-0.9,0.9);
    d<-runif(1,-0.9,0.9);
    f<-b*c; 
    a<-0; 
    e<- -(c*(1-b^2-d^2))/(b*d);

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (inv) return(v) else return(solve(v))
}

gen19<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {
    
    e<-runif(1,-0.9,0.9);
    c<-runif(1,-0.9,0.9);
    d<-runif(1,-0.9,0.9);
    f<-0; a<-0; b<- -d*(1-c^2)/(c*e);

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (inv) return(v) else return(solve(v))
}


gen20<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {
    
    c<-runif(1,-0.9,0.9);
    d<-runif(1,-0.9,0.9);
    e<-runif(1,-0.9,0.9);
    f<-0; a<-0; b<- -c*e/d;

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (!inv) return(v) else return(solve(v))
}

gen21<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {
    
    e<-runif(1,-0.9,0.9);
    c<-runif(1,-0.9,0.9);
    b<-runif(1,-0.9,0.9);
    f<-b*c; a<-0; d<- - e*c*(1-b^2)/(b*(1-c^2));

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (inv) return(v) else return(solve(v))
}


gen22<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {
    
    e<-runif(1,-0.9,0.9);
    c<-runif(1,-0.9,0.9);
    d<-runif(1,-0.9,0.9);
    f<-e*d; a<-0; b<- d*e/c;

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (inv) return(v) else return(solve(v))
}

gen23<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {
    
    e<-runif(1,-0.9,0.9);
    f<-runif(1,-0.9,0.9);
    b<-runif(1,-0.9,0.9);
    d<-e*f; a<-0; c<- b*f;

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (inv) return(v) else return(solve(v))
}

gen24<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {
    
    e<-runif(1,-0.9,0.9);
    d<-runif(1,-0.9,0.9);
    b<-runif(1,-0.9,0.9);
    f<-e*d; a<-0; c<- b*d*e;

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (inv) return(v) else return(solve(v))
}

gen25<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {
    
    e<-runif(1,-0.9,0.9);
    f<-runif(1,-0.9,0.9);
    b<-runif(1,-0.9,0.9);
    d<- -(1-e^2-f^2)/(e*f); a<-0; c<- b*f;

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (inv) return(v) else return(solve(v))
}

gen26<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {
    
    c<-runif(1,-0.9,0.9);
    d<-runif(1,-0.9,0.9);
    e<-runif(1,-0.9,0.9);
    f<-d*e; a<-c*e; b<- c*e/d;

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (inv) return(v) else return(solve(v))
}

gen27<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {
    
    b<-runif(1,-0.9,0.9);
    d<-runif(1,-0.9,0.9);
    f<-runif(1,-0.9,0.9);
    a<-0; c<-b*f; e<- d*(1-b^2*f^2)/(f*(1-b^2));

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (inv) return(v) else return(solve(v))
}

gen28<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {
    
    b<-runif(1,-0.9,0.9);
    c<-runif(1,-0.9,0.9);
    e<-runif(1,-0.9,0.9);
    a<-c*e; d<-c*e*b; f<- c*e^2*b;

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (inv) return(v) else return(solve(v))
}


gen29<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {
    
    d<-runif(1,-0.9,0.9);
    b<-runif(1,-0.9,0.9);
    e<-runif(1,-0.9,0.9);
    c<-0; a<-b*d; f<-d*e;

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (!inv) return(v) else return(solve(v))
}

gen30<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {
    
    d<-runif(1,-0.9,0.9);
    c<-runif(1,-0.9,0.9);
    e<-runif(1,-0.9,0.9);
    a<-0; b<- -c*e/d; f<- 0;

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (inv) return(v) else return(solve(v))
}


gen31<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {
    
    b<-runif(1,-0.9,0.9);
    e<-runif(1,-0.9,0.9);
    f<-runif(1,-0.9,0.9);
    d<-0; c<-b*f; a<-b*f*e;

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (!inv) return(v) else return(solve(v))
}

gen32<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {
    
    c<-runif(1,-0.9,0.9);
    e<-runif(1,-0.9,0.9);
    d<-runif(1,-0.9,0.9);
    f<-0; a<-c*e; b<-c*e/d;

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (!inv) return(v) else return(solve(v))
}


gen33<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {
    
    b<-runif(1,-0.9,0.9);
    c<-runif(1,-0.9,0.9);
    e<-runif(1,-0.9,0.9);
    f<-runif(1,-0.9,0.9);
    a<-0; d<-0;

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (inv) return(v) else return(solve(v))
}


gen34<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {
    
    b<-runif(1,-0.9,0.9);
    c<-runif(1,-0.9,0.9);
    e<-runif(1,-0.9,0.9);
    f<-runif(1,-0.9,0.9);
    a<-0; d<-0;

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (!inv) return(v) else return(solve(v))
}

gen35<-function(tol = 1e-06,inv=TRUE,flip=2*rbinom(1,1,0.5)-1) 
{
  flag<-TRUE;
  while (flag)
  {

    e<-runif(1,-0.9,0.9);
    d<-runif(1,-0.9,0.9);
    a<-0; f<- 0; b<-e*flip; c<- -d*flip;

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (inv) return(v) else return(solve(v))
}

gen36<-function(tol = 1e-06,inv=TRUE,flip=2*rbinom(1,1,0.5)-1) 
{
  flag<-TRUE;
  while (flag)
  {

    e<-runif(1,-0.9,0.9);
    d<-runif(1,-0.9,0.9);
    a<-flip*d*e; f<- d*e; b<-e*flip; c<- d*flip;

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (inv) return(v) else return(solve(v))
}

gen37<-function(tol = 1e-06,inv=TRUE,flip=2*rbinom(1,1,0.5)-1) 
{
  flag<-TRUE;
  while (flag)
  {

    e<-runif(1,-0.9,0.9);
    f<-runif(1,-0.9,0.9);
    a<-0; d<- f*e; b<-flip*sqrt(1-e^2); c<- flip*f*sqrt(1-e^2);

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (inv) return(v) else return(solve(v))
}

gen38<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {
    
    b<-runif(1,-0.9,0.9);
    e<-runif(1,-0.9,0.9);
    f<-runif(1,-0.9,0.9);
    a<-0; c<-0; d<- e*(1-b^2)/f;

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (inv) return(v) else return(solve(v))
}

gen39<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {
    
    c<-runif(1,-0.9,0.9);
    e<-runif(1,-0.9,0.9);
    f<-runif(1,-0.9,0.9);
    a<-0; d<-0; b<- c*f;

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (inv) return(v) else return(solve(v))
}


gen40<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {
    
    a<-runif(1,-0.9,0.9);
    d<-runif(1,-0.9,0.9);
    f<-runif(1,-0.9,0.9);
    c<-0; e<-0; b<-a*(1-f^2)/d;

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (!inv) return(v) else return(solve(v))
}

gen41<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {
    
    b<-runif(1,-0.9,0.9);
    d<-runif(1,-0.9,0.9);
    f<-runif(1,-0.9,0.9);
    c<-0; e<-0; a<-b*d

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (!inv) return(v) else return(solve(v))
}

gen42<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {
    
    b<-runif(1,-0.9,0.9);
    d<-runif(1,-0.9,0.9);
    f<-runif(1,-0.9,0.9);
    a<-0; c<-0; e<-0;

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (inv) return(v) else return(solve(v))
}


gen43<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {
    
    b<-runif(1,-0.9,0.9);
    f<-runif(1,-0.9,0.9);
    d<-runif(1,-0.9,0.9);
    a<-0; c<-0; e<-0

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (!inv) return(v) else return(solve(v))
}

gen44<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {
    
    b<-runif(1,-0.9,0.9);
    e<-runif(1,-0.9,0.9);
    f<-runif(1,-0.9,0.9);
    a<-0; c<-b*f; d<-0;

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (inv) return(v) else return(solve(v))
}

gen45<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {
    
    b<-runif(1,-0.9,0.9);
    e<-runif(1,-0.9,0.9);
    c<-runif(1,-0.9,0.9);
    a<-0; d<-0; f<-0;

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (inv) return(v) else return(solve(v))
}


gen46<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {
    
    b<-runif(1,-0.9,0.9);
    c<-runif(1,-0.9,0.9);
    e<-runif(1,-0.9,0.9);
    a<-0; d<-0; f<-0;

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (!inv) return(v) else return(solve(v))
}

gen47<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {
    
    d<-runif(1,-0.9,0.9);
    e<-runif(1,-0.9,0.9);
    f<-runif(1,-0.9,0.9);
    a<-0; b<-0; c<-0;

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (inv) return(v) else return(solve(v))
}

gen48<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {
    
    
    e<-runif(1,-0.9,0.9);
    f<-runif(1,-0.9,0.9);
    a<-0; b<-0; c<-0; d<-0;

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (inv) return(v) else return(solve(v))
}


gen49<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {

    f<-runif(1,-0.9,0.9);
    e<-runif(1,-0.9,0.9);
    a<-0; b<-0; c<-0; d<-0;

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (!inv) return(v) else return(solve(v))
}

gen50<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {
    
    
    e<-runif(1,-0.9,0.9);
    b<-runif(1,-0.9,0.9);
    a<-0; c<-0; d<-0; f<-0;

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (inv) return(v) else return(solve(v))
}

gen51<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {
    
    
    e<-runif(1,-0.9,0.9);
    b<-0;
    a<-0; c<-0; d<-0; f<-0;

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (inv) return(v) else return(solve(v))
}

gen52<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {
    
    
    e<-0;
    b<-0;
    a<-0; c<-0; d<-0; f<-0;

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (inv) return(v) else return(solve(v))
}

gen53<-function(tol = 1e-06,inv=TRUE) 
{
  flag<-TRUE;
  while (flag)
  {
    a<-runif(1,-0.9,0.9);
    b<-runif(1,-0.9,0.9);
    c<-runif(1,-0.9,0.9);
    d<-runif(1,-0.9,0.9);
    e<-runif(1,-0.9,0.9);
    f<-runif(1,-0.9,0.9);

    v<-c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1);
    
    dim(v)<-c(4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;
  }
  if (inv) return(v) else return(solve(v))
}

generate.data<-function(V=NA,n=1000,Sigma=NA,mu=c(0,0,0,0))
{
  
  if (!is.numeric(mu) || (is.vector(mu) == FALSE) || any(!is.finite(mu)) ||  (length(mu)!=4))
        stop("'mu' must be a vector of four real numbers")

  if (!is.numeric(n) || any(!is.finite(n)) ||  (length(n)!=1) || (n!=trunc(n)) || (n<1))
        stop("'n' must be a positive integer")

  if (any(is.na(Sigma))==TRUE && any(is.na(V))==TRUE) 
        stop("'V' and 'Sigma' cannot be both NA")

  if (any(!is.finite(Sigma))==TRUE && any(!is.finite(V))==TRUE) 
        stop("'V' and 'Sigma' must contain only finite values")
  
  if (any(!is.finite(Sigma))==TRUE) 
  {
      if (!is.numeric(V) || !is.matrix(V) || nrow(V)!=4 || ncol(V)!=4)
        stop("'V' must be a real matrix 4x4")
      if (!isSymmetric(V))
        stop("'V' must be a symmetric matrix")
      if (!is.positive.definite(V)) 
        stop("'V' must be a positive definite matrix")     
     Sigma<-solve(V)
  }
  else {
      if (!is.numeric(Sigma) || !is.matrix(Sigma) || nrow(Sigma)!=4 || ncol(Sigma)!=4)
        stop("'Sigma' must be a real matrix 4x4")
      if (!isSymmetric(Sigma))
        stop("'Sigma' must be a symmetric matrix")
      if (!is.positive.definite(Sigma))  
        stop("'Sigma' must be a positive definite matrix")
  }

  return(mvrnorm(n,mu=mu,Sigma=Sigma))
}

mle52<-function(n,q,tol = 1e-06,nb.trials=10) 
{

  ll<-function()
  { e<-0; b<-0; a<-0; c<-0; d<-0; f<-0;
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(-n*log(det(v))+sum(diag(v%*%q)))};  

  v<-gen52()
  nejlepsi<-v;
  hodnota<-ll();  

  return(nejlepsi)
}


mle51<-function(n,q,tol = 1e-06,nb.trials=10) 
{

  ll<-function(x)
  { e<-x; b<-0; a<-0; c<-0; d<-0; f<-0;
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(-n*log(det(v))+sum(diag(v%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen51();
    op<-try(optim(c(e[2,4]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    e<-x[1]; b<-0; a<-0; c<-0; d<-0; f<-0;
    v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }

  return(nejlepsi)
}

mle50<-function(n,q,tol = 1e-06,nb.trials=10) 
{

  ll<-function(x)
  { e<-x[1]; b<-x[2]; a<-0; c<-0; d<-0; f<-0;
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(-n*log(det(v))+sum(diag(v%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen50();
    op<-try(optim(c(e[2,4],e[1,3]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    e<-x[1]; b<-x[2]; a<-0; c<-0; d<-0; f<-0;
    v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }

  return(nejlepsi)
}

mle49<-function(n,q,tol = 1e-06,nb.trials=10) 
{

  ll<-function(x)
  {e<-x[1]; f<-x[2]; a<-0; b<-0; c<-0; d<-0;
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(n*log(det(v))+sum(diag(solve(v)%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen49(inv=FALSE);
    op<-try(optim(c(e[2,4],e[3,4]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    e<-x[1]; f<-x[2]; a<-0; b<-0; c<-0; d<-0;
    v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }

  return(nejlepsi)
}


mle48<-function(n,q,tol = 1e-06,nb.trials=10) 
{

  ll<-function(x)
  { e<-x[1]; f<-x[2]; a<-0; b<-0; c<-0; d<-0;
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(-n*log(det(v))+sum(diag(v%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen48();
    op<-try(optim(c(e[2,4],e[3,4]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    e<-x[1]; f<-x[2]; a<-0; b<-0; c<-0; d<-0;
    v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }

  return(nejlepsi)
}

mle47<-function(n,q,tol = 1e-06,nb.trials=10) 
{

  ll<-function(x)
  { d<-x[1]; e<-x[2]; f<-x[3]; a<-0; b<-0; c<-0;
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(-n*log(det(v))+sum(diag(v%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen47();
    op<-try(optim(c(e[2,3],e[2,4],e[3,4]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    d<-x[1]; e<-x[2]; f<-x[3]; a<-0; b<-0; c<-0;
    v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }

  return(nejlepsi)
}

mle46<-function(n,q,tol = 1e-06,nb.trials=10) 
{

  ll<-function(x)
  { b<-x[1]; c<-x[2]; e<-x[3]; a<-0; d<-0; f<-0;
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(n*log(det(v))+sum(diag(solve(v)%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen46(inv=FALSE);
    op<-try(optim(c(e[1,3],e[1,4],e[2,4]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    b<-x[1]; c<-x[2]; e<-x[3]; a<-0; d<-0; f<-0;
    v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }

  return(nejlepsi)
}


mle45<-function(n,q,tol = 1e-06,nb.trials=10) 
{


  ll<-function(x)
  { b<-x[1]; e<-x[2]; c<-x[3]; a<-0; f<-0; d<-0;
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(-n*log(det(v))+sum(diag(v%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen45();
    op<-try(optim(c(e[1,3],e[2,4],e[1,4]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    b<-x[1]; e<-x[2]; c<-x[3]; a<-0; f<-0; d<-0;
    v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }

  return(nejlepsi)
}

mle44<-function(n,q,tol = 1e-06,nb.trials=10) 
{

  ll<-function(x)
  { b<-x[1]; e<-x[2]; f<-x[3]; a<-0; c<-b*f; d<-0;
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(-n*log(det(v))+sum(diag(v%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen44();
    op<-try(optim(c(e[1,3],e[2,4],e[3,4]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    b<-x[1]; e<-x[2]; f<-x[3]; a<-0; c<-b*f; d<-0;
    v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }

  return(nejlepsi)
}

mle43<-function(n,q,tol = 1e-06,nb.trials=10) 
{

  ll<-function(x)
  {b<-x[1]; f<-x[2]; d<-x[3];  a<-0; c<-0; e<-0;
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(n*log(det(v))+sum(diag(solve(v)%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen43(inv=FALSE);
    op<-try(optim(c(e[1,3],e[3,4],e[2,3]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    b<-x[1]; f<-x[2]; d<-x[3];  a<-0; c<-0; e<-0;
    v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }

  return(nejlepsi)
}


mle42<-function(n,q,tol = 1e-06,nb.trials=10) 
{

  ll<-function(x)
  { b<-x[1]; d<-x[2]; f<-x[3]; a<-0; c<-0; e<- 0;
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(-n*log(det(v))+sum(diag(v%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen42();
    op<-try(optim(c(e[1,3],e[2,3],e[3,4]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    b<-x[1]; d<-x[2]; f<-x[3]; a<-0; c<-0; e<- 0;
    v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }

  return(nejlepsi)
}

mle41<-function(n,q,tol = 1e-06,nb.trials=10) 
{

  ll<-function(x)
  {b<-x[1]; d<-x[2]; f<-x[3];  c<-0; e<-0; a<-b*d
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(n*log(det(v))+sum(diag(solve(v)%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen41(inv=FALSE);
    op<-try(optim(c(e[1,3],e[2,3],e[3,4]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    b<-x[1]; d<-x[2]; f<-x[3];  c<-0; e<-0; a<-b*d
    v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }

  return(nejlepsi)
}


mle40<-function(n,q,tol = 1e-06,nb.trials=10) 
{

  ll<-function(x)
  {a<-x[1]; d<-x[2]; f<-x[3];  c<-0; e<-0; b<-a*(1-f^2)/d;
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(n*log(det(v))+sum(diag(solve(v)%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen40(inv=FALSE);
    op<-try(optim(c(e[1,2],e[2,3],e[3,4]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    a<-x[1]; d<-x[2]; f<-x[3];  c<-0; e<-0; b<-a*(1-f^2)/d;
    v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }

  return(nejlepsi)
}


mle39<-function(n,q,tol = 1e-06,nb.trials=10) 
{

  ll<-function(x)
  {c<-x[1]; e<-x[2]; f<-x[3]; a<-0; d<-0; b<- c*f;
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(-n*log(det(v))+sum(diag(v%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen39();
    op<-try(optim(c(e[1,4],e[2,4],e[3,4]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    c<-x[1]; e<-x[2]; f<-x[3]; a<-0; d<-0; b<- c*f;
    v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }

  return(nejlepsi)
}

mle38<-function(n,q,tol = 1e-06,nb.trials=10) 
{

  ll<-function(x)
  {b<-x[1]; e<-x[2]; f<-x[3]; a<-0; c<-0; d<- e*(1-b^2)/f;
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(-n*log(det(v))+sum(diag(v%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen38();
    op<-try(optim(c(e[1,3],e[2,4],e[3,4]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    b<-x[1]; e<-x[2]; f<-x[3]; a<-0; c<-0; d<- e*(1-b^2)/f;
    v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }


  return(nejlepsi)
}

mle37<-function(n,q,tol = 1e-06,nb.trials=10) 
{

  ll<-function(x)
  {f<-x[1]; e<-x[2]; a<-0; d<- f*e; b<-sqrt(1-e^2); c<- f*sqrt(1-e^2);
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(-n*log(det(v))+sum(diag(v%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen37(flip=1);
    op<-try(optim(c(e[3,4],e[2,4]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    f<-x[1]; e<-x[2]; a<-0; d<- f*e; b<-sqrt(1-e^2); c<- f*sqrt(1-e^2);
    v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }

  ll<-function(x)
  {f<-x[1]; e<-x[2]; a<-0; d<- f*e; b<- -sqrt(1-e^2); c<- -f*sqrt(1-e^2);
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(-n*log(det(v))+sum(diag(v%*%q)))}; 

 for (i in (1:nb.trials))
  {
    e<-gen37(flip=-1);
    op<-try(optim(c(e[3,4],e[2,4]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    f<-x[1]; e<-x[2]; a<-0; d<- f*e; b<- -sqrt(1-e^2); c<- -f*sqrt(1-e^2);
    v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }

  return(nejlepsi)
}

mle36<-function(n,q,tol = 1e-06,nb.trials=10) 
{

  ll<-function(x)
  {d<-x[1]; e<-x[2]; a<-d*e; f<- d*e; b<-e; c<-d;
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(-n*log(det(v))+sum(diag(v%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen36(flip=1);
    op<-try(optim(c(e[2,3],e[2,4]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    d<-x[1]; e<-x[2]; a<-d*e; f<- d*e; b<-e; c<-d;
    v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }

  ll<-function(x)
  {d<-x[1]; e<-x[2]; a<- -d*e; f<- d*e; b<- -e; c<--d;
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(-n*log(det(v))+sum(diag(v%*%q)))};  

 for (i in (1:nb.trials))
  {
    e<-gen36(flip=-1);
    op<-try(optim(c(e[2,3],e[2,4]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    d<-x[1]; e<-x[2]; a<--d*e; f<- d*e; b<--e; c<--d;
    v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }

  return(nejlepsi)
}

mle35<-function(n,q,tol = 1e-06,nb.trials=10) 
{

  ll<-function(x)
  {d<-x[1]; e<-x[2]; a<-0; f<- 0; b<-e; c<- -d;
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(-n*log(det(v))+sum(diag(v%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen35(flip=1);
    op<-try(optim(c(e[2,3],e[2,4]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    d<-x[1]; e<-x[2]; a<-0; f<- 0; b<-e; c<- -d;
    v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }

  ll<-function(x)
  {d<-x[1]; e<-x[2]; a<-0; f<- 0; b<- -e; c<- d;
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(-n*log(det(v))+sum(diag(v%*%q)))};  


 for (i in (1:nb.trials))
  {
    e<-gen35(flip=-1);
    op<-try(optim(c(e[2,3],e[2,4]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    d<-x[1]; e<-x[2]; a<-0; f<- 0; b<- -e; c<- d;
    v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }

   return(nejlepsi)
}

mle34<-function(n,q,tol = 1e-06,nb.trials=10) 
{

  ll<-function(x)
  {c<-x[1]; b<-x[2]; e<-x[3];  f<-x[4]; a<-0; d<-0;
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(n*log(det(v))+sum(diag(solve(v)%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen34(inv=FALSE);
    op<-try(optim(c(e[1,4],e[1,3],e[2,4],e[3,4]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    c<-x[1]; b<-x[2]; e<-x[3];  f<-x[4]; a<-0; d<-0;
    v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }

  return(nejlepsi)
}

mle33<-function(n,q,tol = 1e-06,nb.trials=10) 
{

  ll<-function(x)
  {b<-x[1]; c<-x[2]; e<-x[3]; f<-x[4]; a<-0; d<- 0;
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(-n*log(det(v))+sum(diag(v%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen33();
    op<-try(optim(c(e[1,3],e[1,4],e[2,4],e[3,4]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    b<-x[1]; c<-x[2]; e<-x[3]; f<-x[4]; a<-0; d<- 0;
    v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }

 return(nejlepsi)
}

mle32<-function(n,q,tol = 1e-06,nb.trials=10) 
{

  ll<-function(x)
  {c<-x[1]; d<-x[2]; e<-x[3];  f<-0; a<-c*e; b<-c*e/d;
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(n*log(det(v))+sum(diag(solve(v)%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen32(inv=FALSE);
    op<-try(optim(c(e[1,4],e[2,3],e[2,4]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    c<-x[1]; d<-x[2]; e<-x[3];  f<-0; a<-c*e; b<-c*e/d;
    v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }

  return(nejlepsi)
}

mle31<-function(n,q,tol = 1e-06,nb.trials=10) 
{

  ll<-function(x)
  {b<-x[1]; e<-x[2]; f<-x[3]; d<-0; c<-b*f; a<-b*f*e;
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(n*log(det(v))+sum(diag(solve(v)%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen31(inv=FALSE);
    op<-try(optim(c(e[1,3],e[2,4],e[3,4]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    b<-x[1]; e<-x[2]; f<-x[3]; d<-0; c<-b*f; a<-b*f*e;
    v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }

  return(nejlepsi)
}

mle30<-function(n,q,tol = 1e-06,nb.trials=10) 
{

  ll<-function(x)
  {d<-x[1]; c<-x[2]; e<-x[3]; a<-0; b<- -c*e/d; f<- 0;
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(-n*log(det(v))+sum(diag(v%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen30();
    op<-try(optim(c(e[2,3],e[1,4],e[2,4]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    d<-x[1]; c<-x[2]; e<-x[3]; a<-0; b<- -c*e/d; f<- 0;
    v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }

  return(nejlepsi)

}

mle29<-function(n,q,tol = 1e-06,nb.trials=10) 
{

  ll<-function(x)
  {b<-x[1]; d<-x[2]; e<-x[3]; c<-0; a<-b*d; f<-d*e;
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(n*log(det(v))+sum(diag(solve(v)%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen29(inv=FALSE);
    op<-try(optim(c(e[1,3],e[2,3],e[2,4]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    b<-x[1]; d<-x[2]; e<-x[3]; c<-0; a<-b*d; f<-d*e;
    v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }

  return(nejlepsi)
}

mle28<-function(n,q,tol = 1e-06,nb.trials=10) 
{

  ll<-function(x)
  {b<-x[1]; c<-x[2]; e<-x[3]; a<-c*e; d<-c*e*b; f<- c*e^2*b;
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(-n*log(det(v))+sum(diag(v%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen28();
    op<-try(optim(c(e[1,3],e[1,4],e[2,4]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    b<-x[1]; c<-x[2]; e<-x[3]; a<-c*e; d<-c*e*b; f<- c*e^2*b;
    v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }

  return(nejlepsi)
}

mle27<-function(n,q,tol = 1e-06,nb.trials=10) 
{

  ll<-function(x)
  {b<-x[1]; d<-x[2]; f<-x[3]; a<-0; c<-b*f; e<- d*(1-b^2*f^2)/(f*(1-b^2));
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(-n*log(det(v))+sum(diag(v%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen27();
    op<-try(optim(c(e[1,3],e[2,3],e[3,4]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    b<-x[1]; d<-x[2]; f<-x[3]; a<-0; c<-b*f; e<- d*(1-b^2*f^2)/(f*(1-b^2));
    v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }

  return(nejlepsi)
}

mle26<-function(n,q,tol = 1e-06,nb.trials=10) 
{

  ll<-function(x)
  {c<-x[1]; d<-x[2]; e<-x[3]; f<-d*e; a<-c*e; b<- c*e/d;
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(-n*log(det(v))+sum(diag(v%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen26();
    op<-try(optim(c(e[1,4],e[2,3],e[2,4]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    c<-x[1]; d<-x[2]; e<-x[3]; f<-d*e; a<-c*e; b<- c*e/d;
    v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }


  return(nejlepsi)
}

mle25<-function(n,q,tol = 1e-06,nb.trials=10) 
{

  ll<-function(x)
  {e<-x[1]; b<-x[2]; f<-x[3]; d<- -(1-e^2-f^2)/(e*f); a<-0; c<- b*f;
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(-n*log(det(v))+sum(diag(v%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen25();
    op<-try(optim(c(e[2,4],e[1,3],e[3,4]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    e<-x[1]; b<-x[2]; f<-x[3]; d<- -(1-e^2-f^2)/(e*f); a<-0; c<- b*f;
    v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }

  return(nejlepsi)

}


mle24<-function(n,q,tol = 1e-06,nb.trials=10) 
{

  ll<-function(x)
  {e<-x[1]; b<-x[2]; d<-x[3]; f<-e*d; a<-0; c<- b*d*e;
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(-n*log(det(v))+sum(diag(v%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen24();
    op<-try(optim(c(e[2,4],e[1,3],e[2,3]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    e<-x[1]; b<-x[2]; d<-x[3]; f<-e*d; a<-0; c<- b*d*e;
    v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }

  return(nejlepsi)

}

mle23<-function(n,q,tol = 1e-06,nb.trials=10) 
{

  ll<-function(x)
  {e<-x[1]; b<-x[2]; f<-x[3]; d<-e*f; a<-0; c<- b*f;
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(-n*log(det(v))+sum(diag(v%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen23();
    op<-try(optim(c(e[2,4],e[1,3],e[3,4]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    e<-x[1]; b<-x[2]; f<-x[3]; d<-e*f; a<-0; c<- b*f;
    v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }

  return(nejlepsi)

}


mle22<-function(n,q,tol = 1e-06,nb.trials=10) 
{

  ll<-function(x)
  {e<-x[1]; c<-x[2]; d<-x[3]; f<-e*d; a<-0; b<- d*e/c;
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(-n*log(det(v))+sum(diag(v%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen22();
    op<-try(optim(c(e[2,4],e[1,4],e[2,3]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    e<-x[1]; c<-x[2]; d<-x[3]; f<-e*d; a<-0; b<- d*e/c;
    v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }

  return(nejlepsi)

}

mle19<-function(n,q,tol = 1e-06,nb.trials=10) 
{

  ll<-function(x)
  {e<-x[1]; c<-x[2]; d<-x[3]; f<-0; a<-0; b<- -d*(1-c^2)/(c*e);
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(-n*log(det(v))+sum(diag(v%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen19();
    op<-try(optim(c(e[2,4],e[1,4],e[2,3]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    e<-x[1]; c<-x[2]; d<-x[3]; f<-0; a<-0; b<- -d*(1-c^2)/(c*e);
    v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }

 return(nejlepsi)

}

mle20<-function(n,q,tol = 1e-06,nb.trials=10) 
{

  ll<-function(x)
  {c<-x[1]; d<-x[2]; e<-x[3];  f<-0; a<-0; b<- -c*e/d;
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(n*log(det(v))+sum(diag(solve(v)%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen20(inv=FALSE);
    op<-try(optim(c(e[1,4],e[2,3],e[2,4]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    c<-x[1]; d<-x[2]; e<-x[3];  f<-0; a<-0; b<- -c*e/d;
    v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }

 return(nejlepsi)
}


mle21<-function(n,q,tol = 1e-06,nb.trials=10) 
{
 
  ll<-function(x)
  {e<-x[1]; c<-x[2]; b<-x[3]; f<-b*c; a<-0; d<- - e*c*(1-b^2)/(b*(1-c^2));
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(-n*log(det(v))+sum(diag(v%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen21();
    op<-try(optim(c(e[2,4],e[1,4],e[1,3]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    e<-x[1]; c<-x[2]; b<-x[3]; f<-b*c; a<-0; d<- - e*c*(1-b^2)/(b*(1-c^2));
    v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }

  return(nejlepsi)

}

mle18<-function(n,q,tol = 1e-06,nb.trials=10) 
{
 
  ll<-function(x)
  {b<-x[1]; c<-x[2]; d<-x[3]; f<-b*c; a<-0; e<- -(c*(1-b^2-d^2))/(b*d);
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(-n*log(det(v))+sum(diag(v%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen18();
    op<-try(optim(c(e[1,3],e[1,4],e[2,3]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    b<-x[1]; c<-x[2]; d<-x[3]; f<-b*c; a<-0; e<- -(c*(1-b^2-d^2))/(b*d);
    v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }

  return(nejlepsi)
}

mle17<-function(n,q,tol = 1e-06,nb.trials=10) 
{

  ll<-function(x)
  {b<-x[1]; e<-x[2]; f<-x[3]; c<-b*f; a<-0; d<-f*(1-b^2)/e;
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(-n*log(det(v))+sum(diag(v%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen17();
    op<-try(optim(c(e[1,3],e[2,4],e[3,4]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    b<-x[1]; e<-x[2]; f<-x[3]; c<-b*f; a<-0; d<-f*(1-b^2)/e;
    v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }


  return(nejlepsi)

}

mle16<-function(n,q,tol = 1e-06,nb.trials=10) 
{

  ll<-function(x)
  {b<-x[1]; e<-x[2]; f<-x[3];  c<-0; d<-0; a<- -b*e*f/(1-f^2);
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(n*log(det(v))+sum(diag(solve(v)%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen16(inv=FALSE);
    op<-try(optim(c(e[1,3],e[2,4],e[3,4]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    b<-x[1]; e<-x[2]; f<-x[3];  c<-0; d<-0; a<- -b*e*f/(1-f^2);
    v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }

  return(nejlepsi)
}


mle15<-function(n,q,tol = 1e-06,nb.trials=10) 
{

  ll<-function(x)
  {b<-x[1]; d<-x[2]; e<-x[3]; f<-x[4];     c<-b*f;  a<-e*b*f;
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(-n*log(det(v))+sum(diag(v%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen15();
    op<-try(optim(c(e[1,3],e[2,3],e[2,4],e[3,4]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    b<-x[1]; d<-x[2]; e<-x[3]; f<-x[4];     c<-b*f;  a<-e*b*f;
    v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }


  return(nejlepsi)

}


mle14<-function(n,q,tol = 1e-06,nb.trials=10) 
{
 

  ll<-function(x)
  {c<-x[1]; d<-x[2]; e<-x[3]; f<-x[4]; a<-c*e;  b<-(c*(1-d^2-e^2+e*d*f)/(f-d*e));
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(-n*log(det(v))+sum(diag(v%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen14();
    op<-try(optim(c(e[1,4],e[2,3],e[2,4],e[3,4]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    c<-x[1]; d<-x[2]; e<-x[3]; f<-x[4]; a<-c*e;  b<-(c*(1-d^2-e^2+e*d*f)/(f-d*e));
    v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }


  return(nejlepsi)

}

mle13<-function(n,q,tol = 1e-06,nb.trials=10) 
{
 

  ll<-function(x)
  {c<-x[1]; d<-x[2]; e<-x[3]; f<-x[4]; a<-c*e;  b<-c*e/d; 
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(-n*log(det(v))+sum(diag(v%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen13();
    op<-try(optim(c(e[1,4],e[2,3],e[2,4],e[3,4]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    c<-x[1]; d<-x[2]; e<-x[3]; f<-x[4]; a<-c*e;  b<-c*e/d;
    v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }


  return(nejlepsi)

}

mle12<-function(n,q,tol = 1e-06,nb.trials=10) 
{

  ll<-function(x)
  {c<-x[1]; d<-x[2]; e<-x[3]; b<-x[4]; a<-0;  f<-0; 
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(-n*log(det(v))+sum(diag(v%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen12();
    op<-try(optim(c(e[1,4],e[2,3],e[2,4],e[1,3]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    c<-x[1]; d<-x[2]; e<-x[3]; b<-x[4]; a<-0;  f<-0;
    v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }

  return(nejlepsi)

}

mle11<-function(n,q,tol = 1e-06,nb.trials=10) 
{

  ll<-function(x)
  {c<-x[1]; b<-x[2]; e<-x[3]; f<-x[4]; a<-0;  d<-e*f; 
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(-n*log(det(v))+sum(diag(v%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen11();
    op<-try(optim(c(e[1,4],e[1,3],e[2,4],e[3,4]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    c<-x[1]; b<-x[2]; e<-x[3]; f<-x[4]; a<-0;  d<-e*f;
    v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }

  return(nejlepsi)

}    

mle10<-function(n,q,tol = 1e-06,nb.trials=10) 
{
 
  ll<-function(x)
  {c<-x[1]; d<-x[2]; e<-x[3]; f<-x[4]; a<-c*e;  b<-(f+d*e*c^2-d*e-c^2*e^2*f)/(c*(1-e^2)); 
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(-n*log(det(v))+sum(diag(v%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen10();
    op<-try(optim(c(e[1,4],e[2,3],e[2,4],e[3,4]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    c<-x[1]; d<-x[2]; e<-x[3]; f<-x[4]; a<-c*e;  b<-(f+d*e*c^2-d*e-c^2*e^2*f)/(c*(1-e^2));
    v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }

  return(nejlepsi)

}    


mle9<-function(n,q,tol = 1e-06,nb.trials=10) 
{

  ll<-function(x)
  {c<-x[1]; d<-x[2]; e<-x[3]; b<-x[4]; a<-0;  f<-0; 
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(n*log(det(v))+sum(diag(solve(v)%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen9(inv=FALSE);
    op<-try(optim(c(e[1,4],e[2,3],e[2,4],e[1,3]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    c<-x[1]; d<-x[2]; e<-x[3]; b<-x[4]; a<-0;  f<-0;
    v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }

  return(nejlepsi)
}

mle8<-function(n,q,tol = 1e-06,nb.trials=10) 
{

  ll<-function(x)
  {c<-x[1]; d<-x[2]; e<-x[3]; b<-x[4]; a<-c*e;  f<-b*c; 
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(-n*log(det(v))+sum(diag(v%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen8();
    op<-try(optim(c(e[1,4],e[2,3],e[2,4],e[1,3]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    c<-x[1]; d<-x[2]; e<-x[3]; b<-x[4]; a<-c*e;  f<-b*c; 
    v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }

 return(nejlepsi)

}    

mle7<-function(n,q,tol = 1e-06,nb.trials=10) 
{
 
  ll<-function(x)
  {c<-x[1]; d<-x[2]; e<-x[3]; b<-x[4]; a<-0;  f<-b*c; 
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(-n*log(det(v))+sum(diag(v%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen7();
    op<-try(optim(c(e[1,4],e[2,3],e[2,4],e[1,3]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    c<-x[1]; d<-x[2]; e<-x[3]; b<-x[4]; a<-0;  f<-b*c; 
    v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }

 return(nejlepsi)

}    

mle6<-function(n,q,tol = 1e-06,nb.trials=10) 
{

  ll<-function(x)
  {c<-x[1]; d<-x[2]; e<-x[3]; b<-x[4]; a<-0;  f<-b*c+d*e; 
  v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(-n*log(det(v))+sum(diag(v%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen6();
    op<-try(optim(c(e[1,4],e[2,3],e[2,4],e[1,3]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    c<-x[1]; d<-x[2]; e<-x[3]; b<-x[4]; a<-0;  f<-b*c+d*e; 
    v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }

  return(nejlepsi)

}    


mle5<-function(n,q,tol = 1e-06,nb.trials=10) 
{

  ll<-function(x)
  {c<-x[1]; d<-x[2]; e<-x[3]; b<-x[4]; a<-0;  f<-(d*(1-c^2)+b*e*c)/e; v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(-n*log(det(v))+sum(diag(v%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen5();
    op<-try(optim(c(e[1,4],e[2,3],e[2,4],e[1,3]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    c<-x[1]; d<-x[2]; e<-x[3]; b<-x[4]; a<-0;  f<-(d*(1-c^2)+b*e*c)/e; v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }

  return(nejlepsi)

}    

mle4<-function(n,q,tol = 1e-06,nb.trials=10) 
{

  ll<-function(x)
  {c<-x[1]; d<-x[2]; e<-x[3]; f<-x[4]; a<-0; b<--c*(d*f-e)/(e*f-d); v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(-n*log(det(v))+sum(diag(v%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen4();
    op<-try(optim(c(e[1,4],e[2,3],e[2,4],e[3,4]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    c<-x[1]; d<-x[2]; e<-x[3]; f<-x[4]; a<-0; b<--c*(d*f-e)/(e*f-d); v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }

  return(nejlepsi)

}    

mle3<-function(n,q,tol = 1e-06,nb.trials=10) 
{

  ll<-function(x)
  {b<-x[1]; c<-x[2]; d<-x[3]; e<-x[4]; f<-x[5]; a<-(c*e+b*d-f*(b*e+c*d))/(1-f^2); v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(-n*log(det(v))+sum(diag(v%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen3();
    op<-try(optim(c(e[1,3],e[1,4],e[2,3],e[2,4],e[3,4]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    b<-x[1]; c<-x[2]; d<-x[3]; e<-x[4]; f<-x[5]; a<-(c*e+b*d-f*(b*e+c*d))/(1-f^2); v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }

  return(nejlepsi)

}    

mle2<-function(n,q,tol = 1e-06,nb.trials=10) 
{

  ll<-function(x)
  {b<-x[1]; c<-x[2]; d<-x[3]; e<-x[4]; f<-x[5]; a<-c*e; v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(-n*log(det(v))+sum(diag(v%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen2();
    op<-try(optim(c(e[1,3],e[1,4],e[2,3],e[2,4],e[3,4]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    b<-x[1]; c<-x[2]; d<-x[3]; e<-x[4]; f<-x[5]; a<-c*e; v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }

 
  return(nejlepsi)

}    

mle1<-function(n,q,tol = 1e-06,nb.trials=10) 
{
 
  ll<-function(x)
  {a<-0; b<-x[1]; c<-x[2]; d<-x[3]; e<-x[4]; f<-x[5]; 
   v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);   return(-n*log(det(v))+sum(diag(v%*%q)))};  

  nejlepsi<-diag(1,4,4);
  hodnota<-Inf;  

  for (i in (1:nb.trials))
  {
    e<-gen1();
    op<-try(optim(c(e[1,3],e[1,4],e[2,3],e[2,4],e[3,4]),ll));  
    if (!is.list(op)) next()

    flag<-TRUE;
    x<-op$par;
    a<-0; b<-x[1]; c<-x[2]; d<-x[3]; e<-x[4]; f<-x[5]; v<-matrix(c(1,a,b,c,a,1,d,e,b,d,1,f,c,e,f,1),4,4);
    eS <- eigen(v, sym = TRUE, only.values = TRUE)
    ev <- eS$values
    if (all(ev >= tol * abs(ev[1]))) 
      flag<-FALSE;

    if (!flag & (op$value<hodnota)) 
    {
      nejlepsi<-v;
      hodnota<-op$value;
    }
  }

  return(nejlepsi)

}    

mle53<-function(n,q,tol = 1e-06,nb.trials=10)
{
return(q/n)
}





kl.div<-function(V0,mu0,V1,mu1=NA,inv=TRUE)
{

  if (!is.numeric(mu0) || (is.vector(mu0) == FALSE) || any(!is.finite(mu0)))
        stop("'mu' must be a vector of real numbers")

  n<-length(mu0)
  if (any(is.na(mu1))&(length(mu1)==1)) mu1<-rep(0,n)

  if (!is.numeric(mu1) || (is.vector(mu1) == FALSE) || any(!is.finite(mu1)))
        stop("'mu1' must be NA or a vector of real numbers")

  if (length(mu1)!=n)
        stop("'mu0' and 'mu1' must have the same length")

  if (any(!is.finite(V0))==TRUE && any(!is.finite(V1))==TRUE) 
        stop("'V0' and 'V1' must contain only finite values")
  
  if (!is.numeric(V0) || !is.matrix(V0) || nrow(V0)!=n || ncol(V0)!=n)
     stop("'V0' must be a real square matrix corresponding to 'mu0'")
  if (!is.numeric(V1) || !is.matrix(V1) || nrow(V1)!=n || ncol(V1)!=n)
     stop("'V1' must be a real square matrix corresponding to 'mu1'")
  if (!isSymmetric(V0))
     stop("'V0' must be a symmetric matrix")
  if (!isSymmetric(V1))
     stop("'V1' must be a symmetric matrix")
  if (!is.positive.definite(V0)) 
        stop("'V0' must be a positive definite matrix") 
  if (!is.positive.definite(V1)) 
        stop("'V1' must be a positive definite matrix")     
  if (length(inv)!=1 || (inv!=TRUE & inv!=FALSE)) 
        stop("'inv' must be TRUE or FALSE") 

  if (!inv) {V0<-solve(V0); V1<-solve(V1)}

1/2*(log(det(V0))-log(det(V1))+sum(diag(V1%*%solve(V0)))+(mu1-mu0)%*%V1%*%(mu1-mu0)-nrow(V0))
}

is.positive.definite<-function (m, tol, method = c("eigen", "chol")) 
{
# part of 'corpcor' package  

  method <- match.arg(method)
    if (!is.matrix(m)) 
        m <- as.matrix(m)
    if (method == "eigen") {
        eval <- eigen(m, only.values = TRUE)$values
        if (is.complex(eval)) {
            warning("Input matrix has complex eigenvalues!")
            return(FALSE)
        }
        if (missing(tol)) 
            tol <- max(dim(m)) * max(abs(eval)) * .Machine$double.eps
        if (sum(eval > tol) == length(eval)) 
            return(TRUE)
        else return(FALSE)
    }
    if (method == "chol") {
        val <- try(chol(m), silent = TRUE)
        if (class(val) == "try-error") 
            return(FALSE)
        else return(TRUE)
    }
}




mle<-list(mle1, mle2, mle3, mle4, mle5, mle6, mle7, mle8, mle9, mle10, mle11, mle12, mle13, mle14, mle15, mle16, mle17, mle18, mle19, mle20, mle21, mle22, mle23, mle24, mle25, mle26, mle27, mle28, mle29, mle30, mle31, mle32, mle33, mle34, mle35, mle36, mle37, mle38, mle39, mle40, mle41, mle42, mle43, mle44, mle45, mle46, mle47, mle48, mle49, mle50, mle51, mle52, mle53)

gen<-list(gen1, gen2, gen3, gen4, gen5, gen6, gen7, gen8, gen9, gen10, gen11, gen12, gen13, gen14, gen15, gen16, gen17, gen18, gen19, gen20, gen21, gen22, gen23, gen24, gen25, gen26, gen27, gen28, gen29, gen30, gen31, gen32, gen33, gen34, gen35, gen36, gen37, gen38, gen39, gen40, gen41, gen42, gen43, gen44, gen45, gen46, gen47, gen48, gen49, gen50, gen51, gen52, gen53)

npar<-rep(0,53);
npar[1:3]<-1;
npar[4:15]<-2;
npar[16:32]<-3;
npar[33:34]<-2;
npar[35:37]<-4;
npar[38:47]<-3;
npar[48:50]<-4;
npar[51]<-5; 
npar[52]<-6;
npar<-14-npar;

ind.rgauss<-function(ind=NA,model=NA,type=NA) 
{
  model<-ind.identification(ind,model,type)$model;
  return(gen[[TR.MODEL[model,1]]]()[PERM[TR.MODEL[model,2],],PERM[TR.MODEL[model,2],]]);
}

ind.mle<-function(data=NA,V=NA,Sigma=NA,n=NA,ind=NA,model=NA,type=NA,tol = 1e-06,nb.trials=10)
{

  logL<-function(v,n,s) 
  {
    w<-s*n;
    return(n/2*log(det(v))-sum(diag(v%*%w))/2);
  }

  if (any(is.na(data))+any(is.na(V))+any(is.na(Sigma))==3)
    stop("'data' or 'V' or 'Sigma' must be other than NA")

  if (!any(is.na(data))) data<-as.matrix(data)

  if (any(!is.finite(data))+any(!is.finite(V))+any(!is.finite(Sigma))==3)
    stop("'data', 'V' and 'Sigma' must contain only finite values")

  if (all(is.finite(data))) 
  {  if (!is.numeric(data) || ncol(data)!=4 || nrow(data)<3)
          stop("'data' must be a four column real matrix with at least three rows")

     if (!is.positive.definite(var(data))) 
          stop("variance matrix of 'data' must not be singular");    

     n<-nrow(data)
     Sigma<-var(data)*(n-1)/n;
     V<-solve(Sigma);
     mu<-apply(data,2,mean);
  } else{

     mu<-NA;

    if (!is.numeric(n) || any(!is.finite(n)) ||  (length(n)!=1) || (n!=trunc(n)) || (n<1))
          stop("'n' must be a positive integer")
  

    if (any(!is.finite(Sigma))==TRUE) 
    {
        if (!is.numeric(V) || !is.matrix(V) || nrow(V)!=4 || ncol(V)!=4)
          stop("'V' must be a real matrix 4x4");
        if (!isSymmetric(V))
          stop("'V' must be a symmetric matrix");
        if (!is.positive.definite(V)) 
          stop("'V' must be a positive definite matrix");     
       Sigma<-solve(V);
    } else{
        if (!is.numeric(Sigma) || !is.matrix(Sigma) || nrow(Sigma)!=4 || ncol(Sigma)!=4)
          stop("'Sigma' must be a real matrix 4x4")
        if (!isSymmetric(Sigma))
          stop("'Sigma' must be a symmetric matrix")
        if (!is.positive.definite(Sigma))  
          stop("'Sigma' must be a positive definite matrix")
       V<-solve(Sigma)
    }
  }

  if (!is.numeric(nb.trials) || any(!is.finite(nb.trials)) ||  (length(nb.trials)!=1) || (nb.trials!=trunc(nb.trials)) || (nb.trials<1))
        stop("'nb.trials' must be a positive integer")
  
  if (!is.numeric(tol) || any(!is.finite(tol)) ||  (length(tol)!=1) || (tol<0))
        stop("'tol' must be a positive real number")

  model<-ind.identification(ind,model,type)$model;
  type<-ind.identification(ind,model,type)$type;

  bck<-diag(sqrt(c(V[1,1],V[2,2],V[3,3],V[4,4])))
  q<-n*(bck)%*%Sigma%*%(bck)

  tck2<-diag(1/sqrt(c(Sigma[1,1],Sigma[2,2],Sigma[3,3],Sigma[4,4])))
  bck2<-diag(sqrt(c(Sigma[1,1],Sigma[2,2],Sigma[3,3],Sigma[4,4])))
  q2<-n*(tck2)%*%Sigma%*%(tck2)

  op<-options()
  options(show.error.messages=FALSE,warn=-1)
  if (TR.MODEL[model,1] %in% c(49,46,43,41,40,34,32,31,29,20,16,9,53)) {
    sigma<-bck2%*%mle[[TR.MODEL[model,1]]](n,q2[INVPERM[TR.MODEL[model,2],],INVPERM[TR.MODEL[model,2],]],tol,nb.trials)[PERM[TR.MODEL[model,2],],PERM[TR.MODEL[model,2],]]%*%bck2;
    v<-solve(sigma)
  }
  else {
    v<-bck%*%mle[[TR.MODEL[model,1]]](n,q[INVPERM[TR.MODEL[model,2],],INVPERM[TR.MODEL[model,2],]],tol,nb.trials)[PERM[TR.MODEL[model,2],],PERM[TR.MODEL[model,2],]]%*%bck;
    sigma<-solve(v)
  }
  ll<-logL(v,n,Sigma);
  options(op)

  return(list(mu=mu,V=v,Sigma=sigma,ll=ll,aic=-2*ll+2*npar[type],bic=-2*ll+log(n)*npar[type]))
}

model.selection<-function(data=NA,V=NA,Sigma=NA,n=NA,tol = 1e-06,nb.trials=10,search.table=TRUE)
{

  if (any(is.na(data))+any(is.na(V))+any(is.na(Sigma))==3)
    stop("'data' or 'V' or 'Sigma' must be other than NA")

  if (!any(is.na(data))) data<-as.matrix(data)

  if (any(!is.finite(data))+any(!is.finite(V))+any(!is.finite(Sigma))==3)
    stop("'data', 'V' and 'Sigma' must contain only finite values")

  if (all(is.finite(data))) 
  {  if (!is.numeric(data) || ncol(data)!=4 || nrow(data)<3)
          stop("'data' must be a four column real matrix with at least three rows")

     if (!is.positive.definite(var(data))) 
          stop("variance matrix of 'data' must not be singular");    

     n<-nrow(data)
     Sigma<-var(data)*(n-1)/n;
     V<-solve(Sigma);
  } else{

    if (!is.numeric(n) || any(!is.finite(n)) ||  (length(n)!=1) || (n!=trunc(n)) || (n<1))
          stop("'n' must be a positive integer")
  

    if (any(!is.finite(Sigma))==TRUE) 
    {
        if (!is.numeric(V) || !is.matrix(V) || nrow(V)!=4 || ncol(V)!=4)
          stop("'V' must be a real matrix 4x4");
        if (!isSymmetric(V))
          stop("'V' must be a symmetric matrix");
        if (!is.positive.definite(V)) 
          stop("'V' must be a positive definite matrix");     
       Sigma<-solve(V);
    } else{
        if (!is.numeric(Sigma) || !is.matrix(Sigma) || nrow(Sigma)!=4 || ncol(Sigma)!=4)
          stop("'Sigma' must be a real matrix 4x4")
        if (!isSymmetric(Sigma))
          stop("'Sigma' must be a symmetric matrix")
        if (!is.positive.definite(Sigma))  
          stop("'Sigma' must be a positive definite matrix")
       V<-solve(Sigma)
    }
  }

  if (!is.numeric(nb.trials) || any(!is.finite(nb.trials)) ||  (length(nb.trials)!=1) || (nb.trials!=trunc(nb.trials)) || (nb.trials<1))
        stop("'nb.trials' must be a positive integer")
  
  if (!is.numeric(tol) || any(!is.finite(tol)) ||  (length(tol)!=1) || (tol<0))
        stop("'tol' must be a positive real number")


  if (length(search.table)!=1 || (search.table!=TRUE & search.table!=FALSE)) 
        stop("'search.table' must be TRUE or FALSE") 

vysledky<-data.frame(aic=rep(NA,629),bic=rep(NA,629))
bestv<-NA;
bestbic<-Inf;
besti<-0;
gbesti<-0;
gbestbic<-Inf;
gbestv<-NA;

for (i in 1:629) {
  www<-ind.mle(V=V,n=n,model=i,tol = tol,nb.trials=nb.trials)
  vysledky[i,1]<-www$aic
  vysledky[i,2]<-www$bic
  if (www$bic<bestbic) {bestbic<-www$bic; bestv<-www$V; besti<-i}
  if (GRAPHICAL[i]&(www$bic<gbestbic)) {gbestbic<-www$bic; gbestv<-www$V; gbesti<-i}
}

iii<-ind.identification(model=besti);

if (search.table) return(list(model=besti,type=iii$type,ind=iii$ind,best.bic=bestbic,V=bestv,Sigma=solve(bestv),search.table=vysledky,gmodel=gbesti,gV=gbestv,gSigma=solve(gbestv)))
else  return(list(model=besti,type=iii$type,ind=iii$ind,best.bic=bestbic,V=bestv,Sigma=solve(bestv),gmodel=gbesti,gV=gbestv,gSigma=solve(gbestv),search.table=vysledky))
}

var.estimate<-function(data=NA,inv=TRUE,tol = 1e-06,nb.trials=10,trim=0.1)
{

make.pd <- function(m, tol=NA)
{

# part of 'corpcor' package as function 'make.positive.definite' 

   es <- eigen(m)
   esv <- es$values
               
   if (is.na(tol))
     tol <- d*max(abs(esv))*.Machine$double.eps 
   delta <-  2*tol 
                                             
   tau <- pmax(0, delta - esv)
   dm <- es$vectors %*% diag(tau, d) %*% t(es$vectors)    
   
   return( m +  dm)
}

  if (any(is.na(data)))
    stop("'data' must not contain NA values")

  data<-as.matrix(data)

  if (any(!is.finite(data)))
    stop("'data' must not contain infinite values")

  if (!is.numeric(data) || ncol(data)<4 || nrow(data)<3)
          stop("'data' must be a real matrix with at least four columns and three rows")

  if (!is.positive.definite(var(data))) 
          stop("variance matrix of 'data' must not be singular");    

  n<-nrow(data)
  m<-ncol(data)
  Sigma<-var(data)*(n-1)/n;
  V<-solve(Sigma);

  if (!is.numeric(nb.trials) || any(!is.finite(nb.trials)) ||  (length(nb.trials)!=1) || (nb.trials!=trunc(nb.trials)) || (nb.trials<1))
        stop("'nb.trials' must be a positive integer")
  
  if (!is.numeric(tol) || any(!is.finite(tol)) ||  (length(tol)!=1) || (tol<0))
        stop("'tol' must be a positive real number")

  if (!is.numeric(trim) || any(!is.finite(trim)) ||  (length(trim)!=1) || (trim<0) || !(trim<0.5))
        stop("'trim' must be a number between 0 and 0.5")

  komb<-combn(m,4)
  nk<-ncol(komb)  
  res<-rep(NA,m*m*nk*6/(m*(m-1)/2))
  dim(res)<-c(m,m,nk*6/(m*(m-1)/2))

  for (i in 1:nk)
  {
    tato<-komb[,i]  
    if (inv) fit<-model.selection(V=V[tato,tato],n=n)$V else fit<-model.selection(Sigma=Sigma[tato,tato],n=n)$Sigma

    for (k in 1:4) res[tato[k],tato[k],1]<-fit[k,k]

    for (k in 1:3)
      for (l in (k+1):4)
      {
        j<-1;
        while (!is.na(res[tato[k],tato[l],j])) j<-j+1
        res[tato[k],tato[l],j]<-fit[k,l]
        res[tato[l],tato[k],j]<-fit[k,l] 
      }  
  }

  res2<-matrix(0,m,m)
  for (k in 1:m)
    for (l in 1:m) 
      res2[k,l]<-mean(res[k,l,],trim=trim,na.rm=TRUE)
  pd<-is.positive.definite(res2)
  
  if (!pd) 
  {
    warning('Result is not positive-definite matrix');
    if (inv) return(list(V=res2,Vpsd=make.pd(V))) else return(list(Sigma=res2,Sigmapsd=make.pd(Sigma)))
  } else if (inv) return(list(Sigma=solve(res2),V=res2)) else return(list(Sigma=res2,V=solve(res2)))
}

ind.test<-function (v,i,tol=0.000001,inv=TRUE) 
{

if (inv)
{
  rw<-unique(c(CI[[i]][[1]],setdiff(1:4,union(CI[[i]][[3]],CI[[i]][[2]]))));
  cl<-unique(c(CI[[i]][[2]],setdiff(1:4,union(CI[[i]][[3]],CI[[i]][[1]]))));
}
else {
  rw<-c(CI[[i]][[1]],CI[[i]][[3]]);
  cl<-c(CI[[i]][[2]],CI[[i]][[3]]);
}


if (abs(det(as.matrix(v[rw,cl])))<tol) return(TRUE)
                        else return(FALSE)

}


ind.model<-function (V,tol=0.000001,inv=TRUE) 
{

  if (any(!is.finite(V))==TRUE) 
        stop("'V' must contain only finite values")
  
  if (!is.numeric(V) || !is.matrix(V) || nrow(V)!=4 || ncol(V)!=4)
        stop("'V' must be a real matrix 4x4")
  if (!isSymmetric(V))
        stop("'V' must be a symmetric matrix")
  if (!is.positive.definite(V)) 
        stop("'V' must be a positive definite matrix")     

   if (!is.numeric(tol) || any(!is.finite(tol)) ||  (length(tol)!=1) || (tol<0))
        stop("'tol' must be a positive real number")

  if (length(inv)!=1 || (inv!=TRUE & inv!=FALSE)) 
        stop("'inv' must be TRUE or FALSE") 

res<-'';

for (i in 1:24)
  if (ind.test(V,i,tol,inv)) res<-paste(res,1,sep='')
                    else res<-paste(res,0,sep='')

return(res);

}


ind.plot<-function(ind=NA,model=NA,type=NA)
{

ind<-ind.identification(ind,model,type)$ind
 
opt<-options()
options(show.error.messages=FALSE,warn=-1)

if(dev.cur() == 1) get(getOption("device"))(width=3.55,height=3.55,rescale="fixed")
op<-par()
par(plt=c(0,1,0,1))
plot(x=c(-0.25,1.25),y=c(-0.25,1.25),axes=FALSE,xlab="",ylab="",type="n",plt=c(0,1,0,1))
points(0,0,pch=19,cex=4)
points(0,1,pch=19,cex=4)
points(1,0,pch=19,cex=4)
points(1,1,pch=19,cex=4)

if (substr(ind,1,1)=="1") lines(x=c(1,0),y=c(1,1),lwd=5);
if (substr(ind,2,2)=="1") lines(x=c(1,1),y=c(0,1),lwd=5)
if (substr(ind,3,3)=="1") lines(x=c(0,1),y=c(0,0),lwd=5)
if (substr(ind,4,4)=="1") lines(x=c(0,0),y=c(1,0),lwd=5)
if (substr(ind,5,5)=="1") lines(x=c(0,1),y=c(1,0),lwd=5)
if (substr(ind,6,6)=="1") lines(x=c(0,1),y=c(0,1),lwd=5)
if (substr(ind,7,7)=="1") {lines(x=c(0.15,0.85),y=c(0.95,0.95),lwd=5); lines(x=c(0.5,0.57),y=c(0.95,0.88),lwd=5)}
if (substr(ind,8,8)=="1") {lines(x=c(0.15,0.85),y=c(0.95,0.95),lwd=5); lines(x=c(0.5,0.43),y=c(0.95,0.88),lwd=5)}
if (substr(ind,9,9)=="1") {lines(y=c(0.15,0.85),x=c(0.95,0.95),lwd=5); lines(y=c(0.5,0.57),x=c(0.95,0.88),lwd=5)}
if (substr(ind,10,10)=="1") {lines(y=c(0.15,0.85),x=c(0.95,0.95),lwd=5); lines(y=c(0.5,0.43),x=c(0.95,0.88),lwd=5)}
if (substr(ind,11,11)=="1") {lines(x=c(0.15,0.85),y=c(0.05,0.05),lwd=5); lines(x=c(0.5,0.57),y=c(0.05,0.12),lwd=5)}
if (substr(ind,12,12)=="1") {lines(x=c(0.15,0.85),y=c(0.05,0.05),lwd=5); lines(x=c(0.5,0.43),y=c(0.05,0.12),lwd=5)}
if (substr(ind,13,13)=="1") {lines(y=c(0.15,0.85),x=c(0.05,0.05),lwd=5); lines(y=c(0.5,0.57),x=c(0.05,0.12),lwd=5)}
if (substr(ind,14,14)=="1") {lines(y=c(0.15,0.85),x=c(0.05,0.05),lwd=5); lines(y=c(0.5,0.43),x=c(0.05,0.12),lwd=5)}
if (substr(ind,15,15)=="1") {lines(x=c(0.1,0.8),y=c(0.2,0.9),lwd=5); lines(x=c(0.4,0.33),y=c(0.5,0.57),lwd=5)}
if (substr(ind,16,16)=="1") {lines(x=c(0.1,0.8),y=c(0.2,0.9),lwd=5); lines(x=c(0.4,0.47),y=c(0.5,0.43),lwd=5)}
if (substr(ind,17,17)=="1") {lines(x=c(0.2,0.9),y=c(0.9,0.2),lwd=5); lines(x=c(0.6,0.67),y=c(0.5,0.57),lwd=5) }
if (substr(ind,18,18)=="1") {lines(x=c(0.2,0.9),y=c(0.9,0.2),lwd=5); lines(x=c(0.6,0.53),y=c(0.5,0.43),lwd=5) }
sss<-seq(from=0,to=1,length=200);
if (substr(ind,19,19)=="1") {lines(x=1-sss/2,y=(sss-0.5)^3/2+1/2^4+1+0.1,lwd=5); lines(x=sss/2,y=(sss-0.5)^3/2+1/2^4+1+0.1,lwd=5)}
if (substr(ind,20,20)=="1") {lines(y=1-sss/2,x=(sss-0.5)^3/2+1/2^4+1+0.1,lwd=5); lines(y=sss/2,x=(sss-0.5)^3/2+1/2^4+1+0.1,lwd=5)}
if (substr(ind,21,21)=="1") {lines(x=1-sss/2,y=-(sss-0.5)^3/2-1/2^4-0.1,lwd=5); lines(x=sss/2,y=-(sss-0.5)^3/2-1/2^4-0.1,lwd=5)}
if (substr(ind,22,22)=="1") {lines(y=1-sss/2,x=-(sss-0.5)^3/2-1/2^4-0.1,lwd=5); lines(y=sss/2,x=-(sss-0.5)^3/2-1/2^4-0.1,lwd=5)}
if (substr(ind,24,24)=="1") {lines(x=0.05+sss/2*0.7+0.15+((sss-0.5)^3/2+1/2^4)/sqrt(2),y=-0.05+sss/2*0.7+0.15-((sss-0.5)^3/2+1/2^4)/sqrt(2),lwd=5)
lines(x=0.05+1-(sss/2*0.7+0.15-((sss-0.5)^3/2+1/2^4)/sqrt(2)),y=-0.05+1-(sss/2*0.7+0.15+((sss-0.5)^3/2+1/2^4)/sqrt(2)),lwd=5)}
if (substr(ind,23,23)=="1") { 
lines(x=-0.05+sss/2*0.7+0.15-((sss-0.5)^3/2+1/2^4)/sqrt(2),y=-0.05+1-(sss/2*0.7+0.15+((sss-0.5)^3/2+1/2^4)/sqrt(2)),lwd=5)
lines(x=-0.05+1-(sss/2*0.7+0.15+((sss-0.5)^3/2+1/2^4)/sqrt(2)),y=-0.05+(sss/2*0.7+0.15-((sss-0.5)^3/2+1/2^4)/sqrt(2)),lwd=5)}
par(op)
options(opt)
}

ind.print<-function(ind=NA,model=NA,type=NA)
{

ind<-ind.identification(ind,model,type)$ind

if (substr(ind,1,1)=='1') cat(paste(' (1,2|0)',"\n"));
if (substr(ind,2,2)=='1') cat(paste(' (2,3|0)',"\n"));
if (substr(ind,3,3)=='1') cat(paste(' (3,4|0)',"\n"));
if (substr(ind,4,4)=='1') cat(paste(' (1,4|0)',"\n"));
if (substr(ind,5,5)=='1') cat(paste(' (1,3|0)',"\n"));
if (substr(ind,6,6)=='1') cat(paste(' (2,4|0)',"\n")); 	
if (substr(ind,7,7)=='1') cat(paste(' (1,2|3)',"\n"));
if (substr(ind,8,8)=='1') cat(paste(' (1,2|4)',"\n"));
if (substr(ind,9,9)=='1') cat(paste(' (2,3|1)',"\n"));
if (substr(ind,10,10)=='1') cat(paste(' (2,3|4)',"\n"));
if (substr(ind,11,11)=='1') cat(paste(' (3,4|2)',"\n"));
if (substr(ind,12,12)=='1') cat(paste(' (3,4|1)',"\n"));
if (substr(ind,13,13)=='1') cat(paste(' (1,4|2)',"\n"));
if (substr(ind,14,14)=='1') cat(paste(' (1,4|3)',"\n"));
if (substr(ind,15,15)=='1') cat(paste(' (2,4|1)',"\n"));
if (substr(ind,16,16)=='1') cat(paste(' (2,4|3)',"\n"));
if (substr(ind,17,17)=='1') cat(paste(' (1,3|2)',"\n"));
if (substr(ind,18,18)=='1') cat(paste(' (1,3|4)',"\n")); 
if (substr(ind,19,19)=='1') cat(paste('(1,2|34)',"\n"));
if (substr(ind,20,20)=='1') cat(paste('(2,3|14)',"\n"));
if (substr(ind,21,21)=='1') cat(paste('(3,4|12)',"\n"));
if (substr(ind,22,22)=='1') cat(paste('(1,4|23)',"\n"));
if (substr(ind,23,23)=='1') cat(paste('(1,3|24)',"\n"));
if (substr(ind,24,24)=='1') cat(paste('(2,4|13)',"\n"));
} 

CI<-list(
list(1,2,NULL),
list(2,3,NULL),
list(3,4,NULL),
list(1,4,NULL),
list(1,3,NULL),
list(2,4,NULL),
list(1,2,3),
list(1,2,4),
list(2,3,1),
list(2,3,4),
list(3,4,2),
list(3,4,1),
list(1,4,2),
list(1,4,3),
list(2,4,1),
list(2,4,3),
list(1,3,2),
list(1,3,4),
list(1,2,c(3,4)),
list(2,3,c(1,4)),
list(3,4,c(1,2)),
list(1,4,c(2,3)),
list(1,3,c(2,4)),
list(2,4,c(1,3))
)

TYPE<-c("100000000000000000000000", "000000010000000000000000", "000000000000000000100000", 
"100000000000000000100000", "100000000000000000010000", "100000000000000000001000", 
"100000000001000000000000", "000000010001000000000000", "000000000000000000101000", 
"000000010000000000001000", "100000000100000000000000", "101000000000000000000000", 
"000000110000000000000000", "000000010000000000000100", "000000010000010000000000", 
"100000000000000000010100", "100000000000010000001000", "100000000001000000000100", 
"101000000000000000010000", "100000000000000000101000", "100000000001000000100000", 
"100000000011000000000000", "100000000100010000000000", "100000000010010000000000", 
"100000000000010000000010", "000000110010000000000000", "100000000000010000010000", 
"000000011010000000000000", "000000010001000000000100", "101000000000000000100000", 
"000000100000100000010000", "000000110000000000001000", "110000101000000000000000", 
"000000010100000000110000", "101000000000000000101000", "000000110011000000000000", 
"100000000100010000001000", "100100010000100000000001", "110000101000000001000000", 
"100000000000010100000101", "000000010000010100000101", "100101010000101000000000", 
"000000100000010100100101", "110000101000010000100100", "111000101110000000000000", 
"000000011101000000111000", "100110110000110011100110", "110110111000110011100110", 
"100110110100110011110110", "111100111111110000111100", "111110111111110011111110", 
"111111111111111111111111", "000000000000000000000000")

PERM<-rbind(
    c(1,2,3,4),
    c(1,2,4,3),
    c(1,3,2,4),
    c(1,3,4,2),
    c(1,4,2,3),
    c(1,4,3,2),
    c(2,1,3,4),
    c(2,1,4,3),
    c(2,3,1,4),
    c(2,3,4,1),
    c(2,4,1,3),
    c(2,4,3,1),
    c(3,1,2,4),
    c(3,1,4,2),
    c(3,2,1,4),
    c(3,2,4,1),
    c(3,4,1,2),
    c(3,4,2,1),
    c(4,1,2,3),
    c(4,1,3,2),
    c(4,2,1,3),
    c(4,2,3,1),
    c(4,3,1,2),
    c(4,3,2,1))

INVPERM<-t(apply(PERM,1,order))

#MODEL<-rep("000000000000000000000000",53*24)
#for (i in 1:53) 
#{ v<-gen[[i]]()
#  for (j in 1:24)  
#    MODEL[(i-1)*24+j]<-ind.model(v[PERM[j,],PERM[j,]])
#}
#MODEL<-unique(MODEL)
#write.table(TYPE,"TYPE.csv",col.names=FALSE,row.names=FALSE)

#MODEL<-scan("MODEL.csv",character(0),quiet=TRUE)

MODEL<-c("000000000000000000100000", "000000000000000000000010", "000000000000000000000100", 
"000000000000000000010000", "000000000000000000000001", "000000000000000000001000", 
"000000100000000000000000", "000000010000000000000000", "000000000000000010000000", 
"000000000000100000000000", "000000000000000001000000", "000000000000010000000000", 
"000000001000000000000000", "000000000000001000000000", "000000000001000000000000", 
"000000000100000000000000", "000000000000000100000000", "000000000010000000000000", 
"100000000000000000000000", "000010000000000000000000", "000100000000000000000000", 
"010000000000000000000000", "000001000000000000000000", "001000000000000000000000", 
"100000000000000000100000", "000010000000000000000010", "000100000000000000000100", 
"010000000000000000010000", "000001000000000000000001", "001000000000000000001000", 
"010000000000000000100000", "000001000000000000100000", "010000000000000000000010", 
"000001000000000000000100", "001000000000000000000010", "001000000000000000000100", 
"000010000000000000100000", "000100000000000000100000", "100000000000000000000010", 
"100000000000000000000100", "000100000000000000000010", "000010000000000000000100", 
"000010000000000000010000", "000100000000000000000001", "100000000000000000010000", 
"100000000000000000000001", "000100000000000000001000", "000010000000000000001000", 
"001000000000000000010000", "001000000000000000000001", "000001000000000000010000", 
"010000000000000000000001", "000001000000000000001000", "010000000000000000001000", 
"001000000000000000100000", "000001000000000000000010", "010000000000000000000100", 
"000100000000000000010000", "000010000000000000000001", "100000000000000000001000", 
"000000000010000000100000", "000000000000000100000010", "000000000100000000000100", 
"000000000001000000100000", "000000000000001000000010", "000000001000000000000100", 
"000000000000010000010000", "000000000000000001000001", "000000000000100000010000", 
"000000000000000010000001", "000000010000000000001000", "000000100000000000001000", 
"000000100010000000000000", "000000010010000000000000", "000000000000000110000000", 
"000000000100100000000000", "000000000000000101000000", "000000000100010000000000", 
"000000100001000000000000", "000000010001000000000000", "000000000000001010000000", 
"000000001000100000000000", "000000000000001001000000", "000000001000010000000000", 
"101000000000000000000000", "000011000000000000000000", "010100000000000000000000", 
"001000100000000000000000", "001000010000000000000000", "000001000000000010000000", 
"010000000000100000000000", "000001000000000001000000", "010000000000010000000000", 
"000100001000000000000000", "000010000000001000000000", "100000000001000000000000", 
"000100000100000000000000", "000010000000000100000000", "100000000010000000000000", 
"000000001000000000100000", "000000000000001000100000", "000000001000000000000010", 
"000000000000001000000100", "000000000001000000000010", "000000000001000000000100", 
"000000000000000010100000", "000000000000100000100000", "000000100000000000000010", 
"000000010000000000000100", "000000000000010000000010", "000000000000000001000100", 
"000000000000000010010000", "000000000000100000000001", "000000100000000000010000", 
"000000010000000000000001", "000000000000010000001000", "000000000000000001001000", 
"000000000010000000010000", "000000000010000000000001", "000000000000000100010000", 
"000000000100000000000001", "000000000000000100001000", "000000000100000000001000", 
"000000000000000000101000", "000000000000000000000011", "000000000000000000010100", 
"000000110000000000000000", "000000000000000011000000", "000000000000110000000000", 
"000000001100000000000000", "000000000000001100000000", "000000000011000000000000", 
"000100100000000000000000", "000010010000000000000000", "000100000000000010000000", 
"000010000000100000000000", "100000000000000001000000", "100000000000010000000000", 
"000001100000000000000000", "010000010000000000000000", "001000000000000010000000", 
"001000000000100000000000", "010000000000000001000000", "000001000000010000000000", 
"000001001000000000000000", "010000000000001000000000", "001000001000000000000000", 
"001000000000001000000000", "010000000001000000000000", "000001000001000000000000", 
"100000000100000000000000", "100000000000000100000000", "000010000100000000000000", 
"000100000000000100000000", "000010000010000000000000", "000100000010000000000000", 
"000000100000100000000000", "000000010000000010000000", "000000000000010010000000", 
"000000000000100001000000", "000000100000000001000000", "000000010000010000000000", 
"000000100000001000000000", "000000011000000000000000", "000000000001000010000000", 
"000000000001100000000000", "000000001000000001000000", "000000000000011000000000", 
"000000001000000100000000", "000000000100001000000000", "000000001010000000000000", 
"000000000010001000000000", "000000000101000000000000", "000000000001000100000000", 
"000000100100000000000000", "000000010000000100000000", "000000000100000010000000", 
"000000000000100100000000", "000000000010000001000000", "000000000010010000000000", 
"010100000000000000100000", "000011000000000000100000", "010100000000000000000010", 
"000011000000000000000100", "101000000000000000000010", "101000000000000000000100", 
"000011000000000000010000", "010100000000000000000001", "101000000000000000010000", 
"101000000000000000000001", "010100000000000000001000", "000011000000000000001000", 
"001000000000100000100000", "001000000000000010100000", "000001000000010000000010", 
"010000000000000001000100", "000001100000000000000010", "010000010000000000000100", 
"001000000000001000100000", "001000001000000000100000", "000001000001000000000010", 
"010000000001000000000100", "000001001000000000000010", "010000000000001000000100", 
"000100000000000100010000", "000010000100000000000001", "000100000010000000010000", 
"000010000010000000000001", "100000000100000000001000", "100000000000000100001000", 
"000100100000000000010000", "000010010000000000000001", "000100000000000010010000", 
"000010000000100000000001", "100000000000000001001000", "100000000000010000001000", 
"000100000010000000100000", "000010000010000000100000", "000100000000000100000010", 
"000010000100000000000100", "100000000000000100000010", "100000000100000000000100", 
"000001000001000000100000", "010000000001000000100000", "001000000000001000000010", 
"001000001000000000000100", "010000000000001000000010", "000001001000000000000100", 
"000001000000010000010000", "010000000000000001000001", "001000000000100000010000", 
"001000000000000010000001", "010000010000000000001000", "000001100000000000001000", 
"100000000000010000010000", "100000000000000001000001", "000010000000100000010000", 
"000100000000000010000001", "000010010000000000001000", "000100100000000000001000", 
"010000000000000000101000", "000001000000000000101000", "010000000000000000000011", 
"000001000000000000010100", "001000000000000000000011", "001000000000000000010100", 
"000010000000000000101000", "000100000000000000101000", "100000000000000000000011", 
"100000000000000000010100", "000100000000000000000011", "000010000000000000010100", 
"101000000000000000100000", "000011000000000000000010", "010100000000000000000100", 
"010100000000000000010000", "000011000000000000000001", "101000000000000000001000", 
"100000000010000000100000", "000010000000000100000010", "000100000100000000000100", 
"100000000001000000100000", "000010000000001000000010", "000100001000000000000100", 
"010000000000010000010000", "000001000000000001000001", "010000000000100000010000", 
"000001000000000010000001", "001000010000000000001000", "001000100000000000001000", 
"000000000011000000100000", "000000000000001100000010", "000000001100000000000100", 
"000000000000110000010000", "000000000000000011000001", "000000110000000000001000", 
"000000001000100000100000", "000000000000001010100000", "000000001000010000000010", 
"000000000000001001000100", "000000100001000000000010", "000000010001000000000100", 
"000000000000000110010000", "000000000100100000000001", "000000100010000000010000", 
"000000010010000000000001", "000000000100010000001000", "000000000000000101001000", 
"000000000001100000100000", "000000000001000010100000", "000000000000011000000010", 
"000000001000000001000100", "000000100000001000000010", "000000011000000000000100", 
"000000000010001000100000", "000000001010000000100000", "000000000001000100000010", 
"000000000101000000000100", "000000001000000100000010", "000000000100001000000100", 
"000000000000100100010000", "000000000100000010000001", "000000000010010000010000", 
"000000000010000001000001", "000000100100000000001000", "000000010000000100001000", 
"000000100000100000010000", "000000010000000010000001", "000000000000010010010000", 
"000000000000100001000001", "000000100000000001001000", "000000010000010000001000", 
"000010000000100000100000", "000100000000000010100000", "100000000000010000000010", 
"100000000000000001000100", "000100100000000000000010", "000010010000000000000100", 
"010000000000001000100000", "000001001000000000100000", "010000000001000000000010", 
"000001000001000000000100", "001000001000000000000010", "001000000000001000000100", 
"100000000000000100010000", "100000000100000000000001", "000010000010000000010000", 
"000100000010000000000001", "000010000100000000001000", "000100000000000100001000", 
"000001100000000000010000", "010000010000000000000001", "001000000000000010010000", 
"001000000000100000000001", "010000000000000001001000", "000001000000010000001000", 
"000000110001000000000000", "000000000000001011000000", "000000001000110000000000", 
"000000110010000000000000", "000000000000000111000000", "000000000100110000000000", 
"000000001100100000000000", "000000000000001110000000", "000000001100010000000000", 
"000000000000001101000000", "000000100011000000000000", "000000010011000000000000", 
"010000000000100000100000", "000001000000000010100000", "010000000000010000000010", 
"000001000000000001000100", "001000100000000000000010", "001000010000000000000100", 
"000010000000001000100000", "000100001000000000100000", "100000000001000000000010", 
"100000000001000000000100", "000100001000000000000010", "000010000000001000000100", 
"000010000000000100010000", "000100000100000000000001", "100000000010000000010000", 
"100000000010000000000001", "000100000100000000001000", "000010000000000100001000", 
"001000100000000000010000", "001000010000000000000001", "000001000000000010010000", 
"010000000000100000000001", "000001000000000001001000", "010000000000010000001000", 
"000000100101000000000000", "000000010001000100000000", "000000000100001010000000", 
"000000001000100100000000", "000000000010001001000000", "000000001010010000000000", 
"000000100010000001000000", "000000010010010000000000", "000000010000000110000000", 
"000000100100100000000000", "000000000000100101000000", "000000000100010010000000", 
"000000001000100001000000", "000000000000011010000000", "000000011000010000000000", 
"000000100000001001000000", "000000100001100000000000", "000000010001000010000000", 
"000000000101100000000000", "000000000001000110000000", "000000000100011000000000", 
"000000001000000101000000", "000000100010001000000000", "000000011010000000000000", 
"000100100010000000000000", "000010010010000000000000", "000100000000000110000000", 
"000010000100100000000000", "100000000000000101000000", "100000000100010000000000", 
"000001100001000000000000", "010000010001000000000000", "001000000000001010000000", 
"001000001000100000000000", "010000000000001001000000", "000001001000010000000000", 
"100000000000000000101000", "000010000000000000000011", "000100000000000000010100", 
"010000000000000000010100", "000001000000000000000011", "001000000000000000101000", 
"010000010000010000000000", "000001100000000001000000", "010000000000100001000000", 
"000001000000010010000000", "001000010000000010000000", "001000100000100000000000", 
"000010010000000100000000", "000100100100000000000000", "100000000010000001000000", 
"100000000010010000000000", "000100000100000010000000", "000010000000100100000000", 
"000010000100001000000000", "000100001000000100000000", "100000000101000000000000", 
"100000000001000100000000", "000100001010000000000000", "000010000010001000000000", 
"001000011000000000000000", "001000100000001000000000", "000001001000000001000000", 
"010000000000011000000000", "000001000001000010000000", "010000000001100000000000", 
"001000110000000000000000", "000001000000000011000000", "010000000000110000000000", 
"000100001100000000000000", "000010000000001100000000", "100000000011000000000000", 
"000000010100000000110000", "000000100000000100100001", "000000000100000001010010", 
"000000000000010100000101", "000000000010000010001010", "000000000010100000001100", 
"000000010000000001100010", "000000100000010000100100", "000000000000100010000110", 
"000000001001000000011000", "000000000001001000001001", "000000001000001000010001", 
"110000101000000000000000", "100001010000001000000000", "010010001000000010000000", 
"000101000000101000000000", "001010000001000001000000", "001100000001010000000000", 
"100010100000000010000000", "100100010000100000000000", "000110000000010001000000", 
"011000000110000000000000", "001001000010000100000000", "010001000100000100000000", 
"101000000000000000101000", "000011000000000000000011", "010100000000000000010100", 
"000000110011000000000000", "000000000000001111000000", "000000001100110000000000", 
"001000001000100000100000", "001000000000001010100000", "000001001000010000000010", 
"010000000000001001000100", "000001100001000000000010", "010000010001000000000100", 
"000100000000000110010000", "000010000100100000000001", "000100100010000000010000", 
"000010010010000000000001", "100000000100010000001000", "100000000000000101001000", 
"000001100000010000100100", "010000010000000001100010", "001000000000100010000110", 
"000100100000000100100001", "000010010100000000110000", "000100000010000010001010", 
"000010000010100000001100", "100000000100000001010010", "100000000000010100000101", 
"001000001000001000010001", "000001001001000000011000", "010000000001001000001001", 
"000000010100000010110000", "000000100000100100100001", "000000100100000001010010", 
"000000010000010100000101", "000000000010010010001010", "000000000010100001001100", 
"000000011000000001100010", "000000100000011000100100", "000000000001100010000110", 
"000000001001000100011000", "000000000101001000001001", "000000001010001000010001", 
"000101000000101000100000", "010010001000000010100000", "001100000001010000000010", 
"001010000001000001000100", "110000101000000000000010", "100001010000001000000100", 
"001001000010000100010000", "011000000110000000000001", "010001000100000100001000", 
"100010100000000010010000", "100100010000100000000001", "000110000000010001001000", 
"000101100000101000000000", "010010011000000010000000", "001100000001010010000000", 
"001010000001100001000000", "110000101000000001000000", "100001010000011000000000", 
"001001001010000100000000", "011000000110001000000000", "010001000101000100000000", 
"100010100100000010000000", "100100010000100100000000", "000110000010010001000000", 
"000000100000010100100101", "000000010100000001110010", "000000000010100010001110", 
"000000001001001000011001", "100101010000101000000000", "110010101000000010000000", 
"001110000001010001000000", "011001000110000100000000", "100100010100100000110000", 
"100010100000000110100001", "000110000100010001010010", "000110000000010101000101", 
"100010100010000010001010", "100100010010100000001100", "100001010000001001100010", 
"110000101000010000100100", "001010010001000001100010", "001100100001010000100100", 
"010010001000100010000110", "000101000000101010000110", "010001000100000101010010", 
"010001000100010100000101", "011000010110000000110000", "001001100010000100100001", 
"011000000110100000001100", "001001000010000110001010", "110000101001000000011000", 
"100001010001001000001001", "010010001000001010010001", "000101001000101000010001", 
"001010000001001001001001", "001100001001010000011000", "000000011101000000111000", 
"000000100001001100101001", "000000001100001001010011", "000000001000011100010101", 
"000000000011001010001011", "000000001011100000011100", "000000010010000011101010", 
"000000100010110000101100", "000000110000000101100011", "000000110100010000110100", 
"000000000000110110000111", "000000000100100011010110", "111000101110000000000000", 
"101001010010001100000000", "010011001100000110000000", "010101000100101100000000", 
"001011000011000101000000", "011100000111010000000000", "101010100001000011000000", 
"101100010001110000000000", "100011110000001010000000", "110100111000100000000000", 
"000111000000111001000000", "010110001000010011000000", "100110110000110011100110", 
"110001111100001100110001", "011010001111000011011010", "001101000011111100001101", 
"100110110100110011110110", "100110110000110111100111", "100110110010110011101110", 
"110001111100001101110011", "110001111100011100110101", "011010011111000011111010", 
"001101100011111100101101", "011010001111100011011110", "001101000011111110001111", 
"110001111101001100111001", "011010001111001011011011", "001101001011111100011101", 
"110110111000110011100110", "100111110000111011100110", "101110110001110011100110", 
"110011111100001110110001", "110101111100101100110001", "111010101111000011011010", 
"101101010011111100001101", "011110001111010011011010", "001111000011111101001101", 
"111001111110001100110001", "011011001111000111011010", "011101000111111100001101", 
"111100111111110000111100", "101011110011001111101011", "010111001100111111010111", 
"111110111111110011111110", "101111110011111111101111", "110111111100111111110111", 
"111011111111001111111011", "111101111111111100111101", "011111001111111111011111", 
"111111111111111111111111", "000000000000000000000000")



TR.MODEL<-structure(c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
2, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 
5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 
6, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 8, 8, 
8, 8, 8, 8, 9, 9, 9, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 
10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 
11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 12, 12, 12, 13, 13, 13, 
13, 13, 13, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 
14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 15, 15, 15, 15, 15, 
15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 
15, 15, 15, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 17, 
17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 
17, 17, 17, 17, 17, 17, 17, 18, 18, 18, 18, 18, 18, 18, 18, 18, 
18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 19, 
19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 20, 20, 20, 20, 20, 
20, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 22, 22, 22, 
22, 22, 22, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 24, 
24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 
24, 24, 24, 24, 24, 24, 24, 25, 25, 25, 25, 25, 25, 25, 25, 25, 
25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 26, 
26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 27, 27, 27, 27, 27, 
27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 
27, 27, 27, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 
28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 29, 29, 29, 29, 29, 
29, 29, 29, 29, 29, 29, 29, 30, 30, 30, 30, 30, 30, 31, 31, 31, 
31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 
31, 31, 31, 31, 31, 32, 32, 32, 32, 32, 32, 33, 33, 33, 33, 33, 
33, 33, 33, 33, 33, 33, 33, 34, 34, 34, 34, 34, 34, 34, 34, 34, 
34, 34, 34, 35, 35, 35, 36, 36, 36, 37, 37, 37, 37, 37, 37, 37, 
37, 37, 37, 37, 37, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 
38, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 40, 40, 40, 
40, 40, 40, 40, 40, 40, 40, 40, 40, 41, 41, 41, 41, 41, 41, 41, 
41, 41, 41, 41, 41, 42, 42, 42, 42, 43, 43, 43, 43, 44, 44, 44, 
44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 
44, 44, 44, 44, 44, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 
45, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 47, 47, 47, 
47, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 49, 49, 49, 
49, 49, 49, 49, 49, 49, 49, 49, 49, 50, 50, 50, 51, 51, 51, 51, 
51, 51, 52, 53, 1, 3, 4, 13, 14, 17, 1, 2, 3, 4, 5, 6, 13, 14, 
17, 19, 20, 23, 1, 3, 4, 13, 14, 17, 1, 3, 4, 13, 14, 17, 1, 
2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 
20, 21, 22, 23, 24, 1, 3, 4, 13, 14, 17, 1, 3, 4, 7, 9, 10, 13, 
14, 15, 16, 17, 18, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 1, 
3, 4, 1, 2, 3, 4, 5, 6, 13, 14, 17, 19, 20, 23, 1, 2, 3, 4, 5, 
6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 
23, 24, 1, 3, 4, 1, 3, 4, 13, 14, 17, 1, 2, 3, 4, 5, 6, 7, 8, 
9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 
1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 
19, 20, 21, 22, 23, 24, 1, 2, 3, 4, 5, 6, 13, 14, 15, 16, 17, 
18, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 
18, 19, 20, 21, 22, 23, 24, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 
12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 1, 2, 3, 
4, 5, 6, 7, 8, 9, 10, 11, 12, 1, 3, 4, 13, 14, 17, 1, 3, 4, 7, 
9, 10, 13, 14, 15, 16, 17, 18, 1, 3, 4, 13, 14, 17, 1, 2, 3, 
4, 5, 6, 13, 14, 15, 16, 17, 18, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 
11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 1, 2, 
3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 
20, 21, 22, 23, 24, 1, 3, 4, 7, 9, 10, 13, 14, 15, 16, 17, 18, 
1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 
19, 20, 21, 22, 23, 24, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 
13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 1, 2, 3, 4, 5, 
6, 7, 8, 9, 10, 11, 12, 1, 3, 4, 13, 14, 17, 1, 2, 3, 4, 5, 6, 
7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 
23, 24, 1, 3, 4, 13, 14, 17, 1, 2, 3, 4, 5, 6, 7, 8, 11, 19, 
20, 21, 1, 2, 3, 4, 5, 6, 7, 8, 11, 19, 20, 21, 1, 3, 4, 1, 3, 
4, 1, 2, 3, 4, 5, 6, 13, 14, 15, 16, 17, 18, 1, 2, 3, 7, 8, 9, 
10, 11, 12, 13, 15, 16, 1, 2, 3, 4, 5, 6, 7, 8, 11, 19, 20, 21, 
1, 2, 3, 4, 5, 6, 13, 14, 17, 19, 20, 23, 1, 2, 3, 4, 5, 6, 13, 
14, 17, 19, 20, 23, 1, 2, 3, 13, 1, 2, 3, 13, 1, 2, 3, 4, 5, 
6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 
23, 24, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 1, 2, 3, 4, 5, 
6, 7, 8, 9, 10, 11, 12, 1, 7, 9, 10, 1, 2, 5, 7, 8, 9, 10, 11, 
12, 19, 21, 22, 1, 2, 5, 7, 8, 9, 10, 11, 12, 19, 21, 22, 1, 
2, 3, 1, 2, 3, 7, 8, 11, 1, 1), .Dim = c(629, 2))


TR.TYPE<-structure(c(3, 2, 1, 4, 5, 6, 10, 8, 12, 7, 14, 9, 13, 11, 15, 
19, 17, 27, 16, 30, 21, 32, 29, 31, 25, 26, 18, 28, 23, 20, 24, 
22, 34, 33, 35, 36, 37, 40, 41, 38, 39, 43, 42, 44, 46, 45, 47, 
49, 48, 50, 51, 52, 53, 1, 2, 1, 1, 15, 17, 17, 8, 1, 17, 19, 
1, 1, 10, 6, 10, 24, 10, 15, 1, 7, 17, 6, 10, 3, 7, 19, 24, 6, 
1, 19, 17, 1, 1, 1, 1, 17, 20, 5, 12, 4, 1, 1, 8, 1, 1, 1, 1, 
1, 1, 1, 1, 1), .Dim = c(53, 2))

TYPE.OF.MODEL<-apply(TR.MODEL,1,function(x) which(TR.TYPE[,1]==x[1]))

ind.identification<-function(ind=NA,model=NA,type=NA)
{

  if (length(ind)!=1 || length(model)!=1 || length(type)!=1)
    stop("length of parameters must be 1")
  
  if (is.na(ind)+is.na(model)+is.na(type)==3) 
    stop("at least one parameter must be other than NA");

if (!is.na(ind))
{
  if (!is.character(ind) || nchar(ind)!=24)
    stop("'ind' must be a string of 24 characters")
  k<-which(ind==MODEL);
  if (length(k)==0) stop("no model matches an independence character 'ind'")
  model<-k;
  type<-TYPE.OF.MODEL[k]
}
else if (!is.na(type))
{
  if (!(type %in% 1:53))
    stop("'type' must be a number 1-53")
  ind<-TYPE[type];
  model<-which(ind==MODEL);
}
else
{
  if (!(model %in% 1:629))
    stop("'model' must be a number 1-629")
  ind<-MODEL[model];
  type<-TYPE.OF.MODEL[model]
}

return(list(ind=ind,model=model,type=type))
}

GR.TR<-(1:53) %in% c(1,12,33,42,45,47,48,50:53);
GRAPHICAL<-GR.TR[TR.MODEL[,1]]



