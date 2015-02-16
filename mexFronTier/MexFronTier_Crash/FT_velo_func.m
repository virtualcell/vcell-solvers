function vel = FT_velo_func(time, coords)

global V0 FX FY NormalX NormalY TotalArea  DomainX DomainY;

if coords(1)<DomainX(1) || coords(1)>DomainX(2) || coords(2)<DomainY(1) || coords(2)>DomainY(2) || time == 0
    vel = zeros(1,2);
else
    v = [FX(coords(1), coords(2)) FY(coords(1), coords(2))];
    n = [NormalX(coords(1), coords(2)) NormalY(coords(1), coords(2))];
    vn = v*n';
    vel = n*(vn + V0/TotalArea);
end
