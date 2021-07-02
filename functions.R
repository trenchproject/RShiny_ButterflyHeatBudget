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

diurnal_radiation_variation=function(doy, solrad, hour, lon, lat){ 
  
  #Calculate solar time
  rd=180/pi;  # factor to convert radians into degrees
  RevAng = 0.21631 + 2 * atan(0.967 * tan(0.0086 * (-186 + doy))); # Revolution angle in radians
  DecAng = asin(0.39795 * cos(RevAng));  # Declination angle in radians      
  
  f=(279.575+0.9856*doy)/rd;  # f in radians
  ET= (-104.7*sin(f)+596.2*sin(2*f)+4.3*sin(3*f)-12.7*sin(4*f)-429.3*cos(f)-2.0*cos(2*f)+19.3*cos(3*f))/3600;   #(11.4) Equation of time
  LC= 1/15* (15 - lon%%15); # longitude correction, 1/15h for each degree e of standard meridian
  hour_sol=12+LC+ET
  
  #W: hour angle of the sun (in radians)
  W= pi*(hour-hour_sol)/12  #Brock 1981
  
  #Ws: sunset hour angle (in radians)
  Ws= acos( -tan(lat/rd) * tan(DecAng))
  
  d=0.409+0.5016*sin(Ws-1.047)
  b=0.6609-0.4767*sin(Ws-1.047) #papers differ in whether sign before 0.4767 is negative or positive
  
  #rG: ratio of hourly to daily global radiation
  rG= pi/24*(d+b*cos(W))*(cos(W)-cos(Ws))/(sin(Ws)-Ws*cos(Ws))   #(Liu and Jordan, Collares-Pereira and Rable 1979)
  #rG= pi/24*(d +b*cos(W)-cos(Ws))/( sin(Ws)-Ws*cos(Ws) ) #Brock 1981
  
  
  solrad_hour=rG*solrad
  
  return(solrad_hour)
}

air_temp_profile= function(T_r, u_r, zr, z0,z,T_s){
  
  stopifnot(u_r>=0, zr>=0, z0>=0, z>=0)
  
  #friction velocity
  u_star=  0.4*u_r/log(zr/z0 +1)  #0.4 is von Karman constant
  #sublayer stanton number
  S_ts= 0.62/(z0*u_star/12)^0.45
  #bulk Stanton number
  S_tb= 0.64/log(zr/z0+1)
  #Temperature at roughness height, z0
  T_z0= (T_r * S_tb +T_s * S_ts)/(S_tb+S_ts)
  #Temperature at local height
  #Inital from Ecography paper but fixed in vignette: T_z= T_z0 + (T_r - T_z0)*log(z/z0+1)
  T_z= T_z0 + (T_r - T_z0)*log(z/z0+1)/log(zr/z0+1)
  return(T_z)
}

direct_solar_radiation= function(lat,doy,elev,t,t0, method="Campbell 1977"){
  
  stopifnot(lat>=-90, lat<=90, doy>0, doy<367, elev>0, t>=0, t<=24, t0>=0, t0<=24)
  
  #estimate needed quantities
  #elliptical longitude
  E= 0.01675 #eccentricity of the earth's orbit about the sun
  
  ELon= (2*pi/365)*(doy-80)+2*E*(sin(2*pi/365*doy)-sin(2*pi/365*80) )
  
  #solar declination angle
  DecAng = asin(0.39795 * sin(ELon))       # Declination angle in radians, McCullough and Porter 1971           
  
  #extraterrestrial solar flux
  S_po = 1.36 #kW m^{-2}
  
  #optimal air mass
  # adjust atmospheric pressure for elevation
  p_a=101.3* exp (-elev/8200)  # kPa, atmospheric pressure from Campbell & Norman (1998)
  
  #geographical latitude
  geo.lat= lat*pi/180
  
  #sin of sun's altitude angle
  sin_alt_ang= sin(geo.lat)*sin(DecAng)+cos(geo.lat)*cos(DecAng)*cos(0.2618*(t-t0))
  
  m_a=p_a/101.3/sin_alt_ang
  
  a=0.83 #transmissivity of atmosphere, between 0 and 1
  #"The atmospheric transmission coefficient. Varies from 0.9 for a very clear atmosphere to around 0.6 for a hazy or smoggy atmosphere. A typical value for a clear day would be around 0.84." (Campbell, 1977)
  
  #radius vector of the earth in atmospheric units (AU)
  #sunr from insol
  T = (doy - 2451545)/36525
  epsilon = (23 + 26/60 + 21.448/3600) - (46.815/3600) * T - 
    (0.00059/3600) * T^2 + (0.001813/3600) * T^3
  M = 357.5291 + 35999.0503 * T - 0.0001559 * T^2 - 4.8e-07 * 
    T^3
  e = 0.016708617 - 4.2037e-05 * T - 1.236e-07 * T^2
  C = (1.9146 - 0.004817 * T - 1.4e-05 * T^2) * sin(degree_to_radian(M)) + 
    (0.019993 - 0.000101 * T) * sin(2 * degree_to_radian(M)) + 0.00029 * 
    sin(3 * degree_to_radian(M))
  v = M + C
  r = (1.000001018 * (1 - e^2))/(1 + e * cos(degree_to_radian(v)))
  
  #-------
  #Campbell 1977
  
  #direct radiation
  if(method=="Campbell 1977") Sb= a^m_a*S_po*sin_alt_ang
  
  #-------
  #Gates 1962
  
  w=6.93 #precipitable water vapour (mm)
  #"The amount of water vapour in the atmosphere in the zenith direction. Varies from I m for very cold dry atmospheres to about 20 mm in warm moist atmosphere. It can get as high as 30 mm." (Gates, 1962)
  
  d=0.896 #haze-dust concentration (particles cm^{-3})
  #"The concentration of dust and other particulates in the air. Number varies from 0.2-3.0. On clear days it will be 0.6-1.0. Around big cities it will be 1.4-2.0." (Gates, 1962)
  
  if(method=="Gates 1962") Sb = (S_po/r^2)*sin_alt_ang*exp(-0.089*(p_a*m_a/101.3)^0.75 -0.174*(w*m_a/20)^0.6 -0.083*(d*m_a)^0.9)
  
  Sb=Sb*1000 #Convert from kW/m^2 to W/m^2
  
  #Set negative values before and after sunrise to zero
  Sb[Sb<0]=0
  
  return(Sb) 
}  

solar_noon <- function(lon, doy, offset=NA){
  
  stopifnot(lon>=-180, lon<=180, doy>0, doy<367)
  
  # Calculate the time of solar noon for each day using longitude correction (LC), equation of time (ET), and a conversion (f)
  f=(279.575+0.9856*doy)  # f in degrees as a function of day of year, p.169 Campbell & Norman 2000
  f[f>360]=f[f>360]-360 #ensure 0 to 360 degrees
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
  
  return(t_0)
}

degree_to_radian <- function(deg) {(deg * pi) / (180)}

partition_solar_radiation=function(method, kt, lat=NA, sol.elev=NA){  
  
  stopifnot(method %in% c("Liu_Jordan", "Orgill_Hollands", "Erbs", "Olyphant", "Spencer", "Reindl-1", "Reindl-2", "Lam_Li"), kt>=0, kt<=1)
  
  # Methods from Wong and Chow (2001, Applied Energy 69:1991-224)
  
  #based on the correlations between the clearness index kt (dimensionless) and the diffuse fraction kd (dimensionless), diffuse coefficient kD (dimensionless) or the direct transmittance kb (dimensionless) where
  #k_t= I_t/I_o, k_d=I_d/I_t, k_D=I_d/I_o, k_b=I_b/I_o,
  #where I_t, I_b, I_d, and I_o are the global, direct, diffuse, and extraterrestial irradiances, respectively
  
  #kd- diffuse fraction
  
  #6.1 Liu and Jordan 
  if(method=="Liu_Jordan") {
    kd= (0.271 -0.294*kt)/kt #kd= (0.384 -0.416*kt)/kt
    if(kd>1) kd=1
  }
  
  #6.2 Orgill and Hollands
  if(method=="Orgill_Hollands"){
    if(kt<0.35) kd= 1-0.249*kt
    if(kt>=0.35 & kt<=0.75) kd= 1.577-1.84*kt
    if(kt>=0.75) kd = 0.177 
  }
  
  #6.3 Erbs et al.
  if(method=="Erbs"){
    if(kt<=0.22) kd= 1-0.09*kt
    if(kt>0.22 & kt<0.8) kd= 0.9511 -0.1604*kt +4.388*kt^2 -16.638*kt^3 +12.336*kt^4
    if(kt>=0.8) kd = 0.165 #Correction from 0.125 for CO from Olyphant 1984
  }
  
  if(method=="Olyphant"){ #Correction for Colorado from Olyphant 1984
    if(kt<=0.22) kd= 1-0.09*kt
    if(kt>0.22 & kt<0.8) kd= 0.9511 -0.1604*kt +4.388*kt^2 -16.638*kt^3 +12.336*kt^4
    if(kt>=0.8) kd = 0.125 
  }
  
  #6.4 Spencer
  if(method=="Spencer"){
    a3= 0.94+0.0118*abs(lat)
    b3= 1.185+0.0135*abs(lat)
    
    #method assumes constant kd if kt outside below range
    kd=NA
    if(kt>=0.35 & kt<=0.75) kd= a3-b3*kt
  }
  
  #6.5 Reindl et al.
  if(method=="Reindl-1"){
    if(kt<=0.3) kd= 1.02-0.248*kt
    if(kt>0.3 & kt<0.78) kd= 1.45-1.67*kt
    if(kt>=0.78) kd = 0.147
  }
  
  if(method=="Reindl-2"){
    if(kt<=0.3) kd= 1.02-0.254*kt
    if(kt>0.3 & kt<0.78) kd= 1.4-1.749*kt+0.177*sin(sol.elev*180/pi)
    if(kt>=0.78) kd = 0.486*kt -0.182*sin(sol.elev*180/pi)
  }
  
  #6.6 Lam and Li
  if(method=="Lam_Li"){
    if(kt<=0.15) kd= 0.977
    if(kt>0.15 & kt<=0.7) kd= 1.237-1.361*kt
    if(kt>0.7) kd = 0.273
  }
  
  #direct and diffuse is c(rad*(1-kd),rad*(kd))
  
  return (kd)
  
}  
