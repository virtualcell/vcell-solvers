function Polygon = MoveFront(Polygon, PosX, PosY, Ux, Uy, InsideOldIdx, dt, A)

X = PosX(InsideOldIdx);
Y = PosY(InsideOldIdx);
UX = Ux(InsideOldIdx);
UY = Uy(InsideOldIdx);

Tangents = [Polygon(end,:); Polygon(1:end-1,:)] - [Polygon(2:end,:); Polygon(1,:)];
Normals = [-Tangents(:,2), Tangents(:,1)]./repmat(sqrt(Tangents(:,1).^2+Tangents(:,2).^2),1,2);
FX = TriScatteredInterp(X, Y, UX,'nearest');
FY = TriScatteredInterp(X, Y, UY,'nearest');
vel = zeros(size(Polygon));
vel(:,1) = FX(Polygon(:,1), Polygon(:,2)) + 1*Normals(:,1)/A;
vel(:,2) = FY(Polygon(:,1), Polygon(:,2)) + 1*Normals(:,2)/A;
Polygon = Polygon + dt*vel;
