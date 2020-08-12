Tb_butterfly=function(T_a, Tg, Tg_sh, u, H_sdir, H_sdif, z, D, delta, alpha, r_g=0.3, shade=FALSE){
  
  TaK= T_a+273.15 #ambient temperature in K
  TaK_sh=TaK
  Tg= Tg+273.15 #ground surface temperature in K
  Tg_sh= Tg_sh+273 #shaded ground surface temperature in K
  
  u= u *100;  #u- wind speed, convert m/s to cm/s
  H_sdir=H_sdir/10 #divide by ten to convert W/m2 to mW/cm2
  H_sdif=H_sdif/10 #divide by ten to convert W/m2 to mW/cm2
  
  #Total solar radiation
  H_sttl= H_sdir + H_sdif
  
  #Butterfly Parameters
  delta<- delta/10     #delta- thoracic fur thickness, cm
  
  epsilon_s=0.97; #surface emisivity, ranges from 0.95-1
  sigma= 5.67*10^-9; #Stefan-Boltzman constant, mW cm^-2 K^04 or 5.67*10^-8 W m-2 K-4
  Ep=1; #Ep- butterfly thermal emissivity
  
  k_e= 1.3; #k_e- thermal conductivity of the fur, 1.3mWcm^-1*K^-1
  r_i=0.15; #r_i- body radius #Kingsolver 1983
  k_a=0.25; #approximate thermal conductivity of air, mWcm^-1*K^-1
  
  v=15.68*10^-2  #cm^2/s, kinematic viscocity of air,  at 300K http://www.engineeringtoolbox.com/air-absolute-kinematic-viscosity-d_601.html
  
  #---------------------------------------------
  
  #Areas, cm^2
  #Calculate total surface area as area of cylinder without ends
  A_sttl= pi*D*2 #2 in length  #cm^2
  
  #For butterflies basking with wings perpendicular to radiation 
  ##A_s,dir, A_s,ref, A_s,ttl- direct, reflected, and total solar radiative heat transfer surface areas 
  A_sdir= A_sttl/2
  A_sref=A_sdir
  
  #RADIATIVE HEAT FLUx, mW
  Q_s= alpha*A_sdir*H_sdir*cos(z*pi/180)+alpha*A_sref*H_sdif+alpha*r_g*A_sref*H_sttl  
  
  #---------------------------------------------		 
  #THERMAL RADIATIVE FLUX
  #Tsky=0.0552*(TaK)^1.5; #Kelvin, black body sky temperature from Swinbank (1963), 
  Tsky= (1.22*T_a -20.4)+273.15 #K, Gates 1980 Biophysical ecology based on Swnback 1960, Kingsolver (1983) estimates using Brunt equation
  
  #Q_t= 0.5* A_sttl * Ep * sigma * (Tb^4 - Tsky^4) +0.5* A_sttl * Ep * sigma * (Tb^4 - Tg^4)
  
  #---------------------------------------------   	               
  # CONVECTIVE HEAT FLUX
  
  #Reynolds number- ratio of interval viscous forces
  R_e=u*D/v
  #Nusselt number- dimensionless conductance
  N_u=0.6*R_e^0.5
  #N_u=2.3; #Kingsolver 1983;
  
  h_c=N_u*k_a/D;
  h_T=(1/h_c+(r_i+delta)*log((r_i+delta)/r_i)/k_e)^-1;  # h_T- total convective heat tranfer coefficient
  #A_c=A_sttl; #A_c- convective heat transfer surface area
  #Q_c= h_T* A_c* (Tb-T_a);     
  #---------------------------------------------   	 
  #HEAT BUDGET              
  
  # Kingsolver 1983
  #Q_s- total radiative heat flux; Q_t- thermal radiative heat flux; Q_c- convective heat flux
  #Q_s=Q_t + Q_c;
  
  #ADJUST PARAMETERS IF SHADE
  if(shade==TRUE){
    #Calculate without basking by dividing areas by two
    A_sttl=A_sttl/2
    #RADIATIVE HEAT FLUX IN SHADE, mW
    A_sdir= A_sttl/2
    A_sref=A_sdir; 
    H_sdir_sh= 0; #No direct radiation
    H_sdif_sh= H_sdif
    H_sttl= H_sdif + H_sdif_sh #only diffuse and reflected
    Q_s= alpha*A_sdir*H_sdir_sh*cos(z*pi/180)+alpha*A_sref*H_sdif_sh+alpha*r_g*A_sref*H_sttl; 
    Tg= Tg_sh #use shaded surface temperature if shade
  }
  
  #t solved in wolfram alpha #Solve[a t^4 +b t -d, t]
  a<- A_sttl * Ep *sigma
  b<-h_T * A_sttl
  d<- h_T*A_sttl*TaK +0.5*A_sttl * Ep *sigma*Tsky^4 +0.5*A_sttl * Ep *sigma*(Tg)^4 +Q_s
  
  {Te=1/2*sqrt((2*b)/(a*sqrt((sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)/(2^(1/3)*3^(2/3)*a)-(4*(2/3)^(1/3)*d)/(sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)))-(sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)/(2^(1/3)*3^(2/3)*a)+(4*(2/3)^(1/3)*d)/(sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3))-1/2*sqrt((sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)/(2^(1/3)*3^(2/3)*a)-(4*(2/3)^(1/3)*d)/(sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)) }
  #IMPROVE SOLUTION?
  
  return(Te-273.15)
} 

diurnal_temp_variation_sine=function(T_max, T_min, t){
  
  W=pi/12;
  gamma= 0.44 - 0.46* sin(0.9 + W * t)+ 0.11 * sin(0.9 + 2 * W * t);   # (2.2) diurnal temperature function
  T = T_max*gamma + T_min * (1 - gamma)
  
  return(T)
}


zenith_angle=function(doy, lat, lon, hour, offset=NA){
  
  lat=lat*pi/180 #to radians
  
  RevAng = 0.21631 + 2 * atan(0.967 * tan(0.0086 * (-186 + doy))); # Revolution angle in radians
  DecAng = asin(0.39795 * cos(RevAng));                            # Declination angle in radians           
  
  f=(279.575+0.9856*doy)  # f in degrees as a function of day of year, p.169 Campbell & Norman 2000
  f=f*pi/180 #convert f in degrees to radians
  ET= (-104.7*sin (f)+596.2*sin (2*f)+4.3*sin (3*f)-12.7*sin (4*f)-429.3*cos (f)-2.0*cos (2*f)+19.3*cos (3*f))/3600   # (11.4) Equation of time: ET is a 15-20 minute correction which depends on calendar day
  lon[lon<0]=360+lon[lon<0] #convert to 0 to 360
  LC= 1/15*(lon%%15) # longitude correction, 1/15h for each degree of standard meridian
  LC[LC>0.5]= LC[LC>0.5]-1
  t_0 = 12-LC-ET # solar noon
  
  #Check if offset is as expected. (Is the timezone of the location the same as that of the meridian 
  #that's within 7.5 degrees from that location?)
  lon[lon>180]=lon[lon>180]-360
  if (!is.na(offset)) {
    offset_theory <- as.integer(lon / 15) + lon / abs(lon) * as.integer(abs(lon) %% 15 / 7.5)
    t_0 = t_0 - offset_theory + offset
  }
  
  cos.zenith= sin(DecAng)*sin(lat) + cos(DecAng)*cos(lat)*cos(pi/12*(hour-t_0)); #cos of zenith angle in radians
  zenith=acos(cos.zenith)*180/pi # zenith angle in degrees
  zenith[zenith>90]=90 # if measured from the vertical psi can't be greater than pi/2 (90 degrees)
  
  return(zenith)
}

day_of_year<- function(day, format="%Y-%m-%d"){
  day=  as.POSIXlt(day, format=format)
  return(as.numeric(strftime(day, format = "%j")))
}
