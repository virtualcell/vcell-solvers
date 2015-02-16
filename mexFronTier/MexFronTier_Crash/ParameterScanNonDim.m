hold on
for i = -4:(0.25):4
v_bar   = 10.^(-4:(0.25):4);

d = 10^(1);
ksi_bar = 10^(i);

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

v0_bar = v_bar.*(b-a)./(b+a);
m0_bar = 2*v_bar./(a+b);

plot(log10(v0_bar), log10(v_bar) ,'.-')
end