function Front = PropogateFront(FrontierPointer, time, Front, ExactPolygon, Domain)

if isempty(ExactPolygon)
    [~, Front.Polygon] = mexFT_Step_mute(FrontierPointer, time);
    MyCurve = Front.Polygon(:,1)>Domain{1}(1) & Front.Polygon(:,1)<Domain{1}(2) & Front.Polygon(:,2)>Domain{2}(1) & Front.Polygon(:,2)<Domain{2}(2);
    Front.Polygon = Front.Polygon(MyCurve,:);
    Front.LevelSetFunction = [];%@(x,y,t) p_poly_dist_my(x,y,Front.Polygon(:,1), Front.Polygon(:,2));
else
    Front.Polygon = [ExactPolygon{1}(time), ExactPolygon{2}(time)];
end
