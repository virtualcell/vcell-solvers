function d = p_poly_dist_my(x, y, xv, yv) 

NumPoints = length(x);

% d = zeros(NumPoints,1);
% for i = 1:NumPoints
%     d(i) = p_poly_dist(x(i), y(i), xv, yv);
% end
% return;

NumVertices = length(xv);

% If (xv,yv) is not closed, close it.
xv = xv(:);
yv = yv(:);
Nv = length(xv);
if ((xv(1) ~= xv(Nv)) || (yv(1) ~= yv(Nv)))
    xv = [xv; xv(1)];
    yv = [yv; yv(1)];
    Nv = Nv + 1;
end

% linear parameters of segments that connect the vertices
A = -diff(yv);
B =  diff(xv);
C = yv(2:end).*xv(1:end-1) - xv(2:end).*yv(1:end-1);

A = repmat(A, 1, NumPoints);
B = repmat(B, 1, NumPoints);
C = repmat(C, 1, NumPoints);

x_bar = repmat(x', NumVertices, 1);
y_bar = repmat(y', NumVertices, 1);

% find the projection of point (x,y) on each rib
AB = 1./(A.^2 + B.^2);
vv = (A.*x_bar +B.*y_bar+C);
xp = x_bar - (A.*AB).*vv;
yp = y_bar - (B.*AB).*vv;

% find all cases where projected point is inside the segment
idx_x = (((xp>=repmat(xv(1:end-1), 1, NumPoints)) & (xp<=repmat(xv(2:end), 1, NumPoints))) | ((xp>=repmat(xv(2:end), 1, NumPoints)) & (xp<=repmat(xv(1:end-1), 1, NumPoints))));
idx_y = (((yp>=repmat(yv(1:end-1), 1, NumPoints)) & (yp<=repmat(yv(2:end), 1, NumPoints))) | ((yp>=repmat(yv(2:end), 1, NumPoints)) & (yp<=repmat(yv(1:end-1), 1, NumPoints))));
idx = idx_x & idx_y;

% distance from point (x,y) to the vertices
dv = sqrt((repmat(xv(1:end-1), 1, NumPoints)-x_bar).^2 + (repmat(yv(1:end-1), 1, NumPoints)-y_bar).^2);
dp = zeros(NumVertices, NumPoints)+Inf;

if(~any(idx)) % all projections are outside of polygon ribs
   d = min(dv);
else
   % distance from point (x,y) to the projection on ribs
   dp(idx) = sqrt((xp(idx)-x_bar(idx)).^2 + (yp(idx)-y_bar(idx)).^2);
   d = min([dv; dp]);
end

d = d'.*(1-2*inpolygon(x, y, xv, yv));

