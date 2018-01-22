pro rho_uu_data


;rho=fltarr(7,7)
uux=fltarr(1,7)
uuy=fltarr(1,7)
OPENR, 1, 'density_velocity.txt'
READF,1,uux
print, uux
close, 1
end

