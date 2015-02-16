hold on
for i = -2:(0.25):2
    
v = 10^i;

config.D               = 0.1;                        
config.Du              = 0.5;                        
config.ksi             = 0.5;                        
d                      = config.Du/config.D;

L = 10.^(-2:(0.25):2);

ksi_bar = config.ksi*L.^2/config.Du;
v_bar   = v*L/config.Du;

lambda1 = 1/2*(sqrt(v_bar.^2 + 4*ksi_bar) + v_bar);
lambda2 = 1/2*(sqrt(v_bar.^2 + 4*ksi_bar) - v_bar);
A       = 1./(v_bar.^2*d*(d-1) - ksi_bar);
B       = - ksi_bar;
C       = v_bar*d;

s2      = exp(lambda2); 
s1      = exp(-lambda1); 
s       = exp(-C);

b       = A.*(B.*((s2-s)./(s2-s1)./lambda2+(s1-s)./(s2-s1)./lambda1)+C);
a       = A.*(B.*(s1.*(s2-s)./(s2-s1)./lambda2+s2.*(s1-s)./(s2-s1)./lambda1)+C.*s);

m0_bar = 2*v_bar./(a+b);
v0_bar = v_bar.*(b-a)./(b+a);

plot(log10(m0_bar), log10(v0_bar) ,'.-')
% plot((m0_bar), (v0_bar) ,'.-')
end
% xlim([0 100])
% ylim([0 100])
% V0 = v0_bar*config.Du;
% M_tot = config.Du^2/L*m0_bar*(1-exp(-v_bar*d))/v_bar/d;