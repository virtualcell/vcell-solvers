function [FrontierPointer, Front] = InitializeFront(Front, ExactPolygon, Domain, N)

if isempty(ExactPolygon)
    [FrontierPointer, ~, Front.Polygon] = mexFT_Step_mute(2, Domain{1}, Domain{2}, N, 0.0);
    MyCurve = Front.Polygon(:,1)>Domain{1}(1) & Front.Polygon(:,1)<Domain{1}(2) & Front.Polygon(:,2)>Domain{2}(1) & Front.Polygon(:,2)<Domain{2}(2);
    Front.Polygon = Front.Polygon(MyCurve,:);
    Front.LevelSetFunction = @(x,y,t) p_poly_dist_my(x,y,Front.Polygon(:,1), Front.Polygon(:,2));
else
    Front.Polygon = [ExactPolygon{1}(0), ExactPolygon{2}(0)];
    FrontierPointer = [];
end

