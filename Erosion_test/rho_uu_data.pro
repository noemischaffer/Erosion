pro rho_uu_data


;rho=fltarr(7,7)
Nx=0
Ny=0
openr,1,'density_velocity.txt'
readf,1,Nx
readf,1,Ny
uux=fltarr(Nx,Ny,1)
uuy=fltarr(Nx,Ny,1)
readf,1,uux
readf,1,uuy
print, Nx
x=[0,Nx]
parabola=plot(x, uux, xtitle='!8x', ytitle='!8u!D!8x')
close,1
end

