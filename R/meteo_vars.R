#'Potential temperature
#'@description calculates the potential temperature with standard values for cp, cv and p0.
#'@param Ta actual temperature in K
#'@param p pressure in kPa
#'@return theta in K
#'@export
theta<-function(Ta,p){
cp=1005 #J/kgK
cv=717 #J/kgK
p0=100 #kPa

kd=(cp-cv)/cp
theta = Ta*(p0/p)^kd
return(theta)
}

#'Virtual Potential temperature
#'@description calculates the potential temperature with standard values for cp, cv and p0.
#'@param Ta actual temperature in K
#'@param p pressure in kPa
#'@param reh relative humidity in \%
#'@return theta in K
#'@export
thetav<-function(Ta,p,reh){
  cp=1005 #J/kgK
  cv=717 #J/kgK
  p0=100 #kPa
  es0=0.611 #kPa
  lv=2.5*10^6 #latent heat of vaporization J/Kg
  Rv=461.5 #J K kg-1
  T0=273.15 #K

  kd=(cp-cv)/cp
  es=es0*exp((lv/Rv)*(1/T0-1/Ta))
  # print(es)
  e=es*(reh/100) #vapor pressure
  q=0.662*(e/p) #specific humidity
  # print(q)
  # r=(0.622*e)/(p-e) #mixing ratio

  thetav = Ta*(1+0.61*q)*(p0/p)^kd
  return(thetav)
}

#'Friction velocity
#'@description calculates the friction velocity assuming a constant air density at the surface
#'@param tauu eastward stress component
#'@param tauv southern stress component
#'@return uf friction velocity m/s
#'@export
u_friction<-function(tauu,tauv){
rho=1.225 #air density at the surface as constant [kg/m3]

tau<-sqrt(tauu^2*tauv^2) #tauu in [kg m-1 s-1]
uf<-sqrt(tau/rho)
return(uf)
}

#'Kinematic potential temperature flux
#'@description Calculates the kinematic potential temperature flux from the sensible and latent heat fluxes, using
#'equation 2.33, p.12, from the Thesis of Coen Hennipman (TUD).
#'@param Ta actual temperature in K
#'@param p pressure in kPa
#'@param reh relative humidity in \%
#'@param H is variable hfss is WINS50
#'@param LE is variable hfls_eva+hfls_sbl
#'@export
kinematic_heat_flux<-function(Ta,p,reh,H,LE){
  cp=1005 #J/kgK
  cv=717 #J/kgK
  p0=100 #kPa
  es0=0.611 #kPa
  lv=2.5*10^6 #latent heat of vaporization J/Kg
  Rv=461.5 #J K kg-1
  T0=273.15 #K
  rho=1.225 #air density at the surface as constant [kg/m3]

  kd=(cp-cv)/cp
  es=es0*exp((lv/Rv)*(1/T0-1/Ta))
  # print(es)
  e=es*(reh/100) #vapor pressure
  q=0.662*(e/p) #specific humidity

wtheta=H/(rho*cp) #(K m s-1)
wq=LE/(rho*lv) #(kg kg-1 m s-1)

khf<-wtheta*(1+0.61*q)+0.61*wq
return(khf)
}

#'Mixing ratio
#'@param p pressure
#'@param reh relative humidity
#'@param Ta actual temperature
#'
#'@export
mixing_ratio<-function(Ta,p,reh){
  cp=1005 #J/kgK
  cv=717 #J/kgK
  es0=0.611 #kPa
  lv=2.5*10^6 #latent heat of vaporization J/Kg
  Rv=461.5 #J K kg-1
  T0=273.15 #K

  kd=(cp-cv)/cp
  es=es0*exp((lv/Rv)*(1/T0-1/Ta))
  # print(es)
  e=es*(reh/100) #vapor pressure

  r=(0.622*e)/(p-e)
  return(r)
}

#'Virtual temperature
#'@param p pressure
#'@param reh relative humidity
#'@param Ta actual temperature
#'@export
virtual_temperature<-function(Ta,p,reh){
eta=0.622
r<-mixing_ratio(Ta,p,reh)
Tv<-Ta(1+r/eta)/(1+rv)
return(Tv)
}

#'Obukhov length
#'@description calculate the Obukhov length
#'@param khf kinematic heat flux
#'@param p pressure
#'@param reh relative humidity
#'@param Ta actual temperature
#'@param z height
#'@param tauu stress in the u dir
#'@param tauv stress in the v dir
#'@param H sensible heat
#'@param LE latent heat
#'@export
obukhov<-function(z,Ta,p,reh,tauu,tauv,H,LE){
  g=9.81
  k=0.4
  rho=1.225
  cp=1005 #J/kgK
  tv<-thetav(Ta=Ta,p=p,reh=reh)
  uf<-u_friction(tauu=tauu,tauv=tauv)
  khf<-kinematic_heat_flux(Ta,p,reh,H,LE)
  OL<--z*k*(g/tv)*(khf/(rho*cp))*1/uf^3
  return(OL)


}

#'Richardson Gradient number
#'@description Linear approximation of the richardson gradient number for a height difference z
#'@param z height levels corresponding to the model/measurement levels of thetav,u,v.
#'@param thetav virtual potential temperature
#'@param u wind speed eastward
#'@param v wind speed southward
#'@param g gravitational constant
#'@export
Ri_grad<-function(thetav,u,v,z,g=9.81){
  thetav_avr<-mean(thetav)
  dtheta_dz <- mean(diff(thetav)/diff(z))
  dv_dz<-mean(diff(v)/diff(z))
  du_dz<-mean(diff(u)/diff(z))

  Ri <- (g/thetav_avr*dtheta_dz)/(du_dz^2+dv_dz^2)
  return(Ri)
}
