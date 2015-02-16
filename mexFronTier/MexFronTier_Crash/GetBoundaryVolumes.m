function [S, Area, VerticesCoords] = GetBoundaryVolumes(Idx, MeshSize,...
    R, V, NatNeighborPairs, Directions, MidPoints, Front)

NumBoundaryPts = size(Idx,1);
S = zeros(size(NatNeighborPairs,1),1);
Area = zeros(NumBoundaryPts, 1);
VerticesCoords = cell(1, NumBoundaryPts);

polyFront.x = Front.Polygon(:,1);
polyFront.y = Front.Polygon(:,2);

Tolerance = MeshSize*1e-9;
for i = 1:NumBoundaryPts
    Mask = NatNeighborPairs(:,2) == Idx(i);
    DirectionsTemp = Directions(Mask, :);
    MidPointsTemp = MidPoints(Mask, :);
    NumMidPts = size(MidPointsTemp,1);
    
    polyVoronoiTemp.x = V(R{i},1);
    polyVoronoiTemp.y = V(R{i},2);
    
% Optimize the size of the front input (reduce the number of nodes)   
    
    polyFrontTempX = polyFront.x;
    polyFrontTempY = polyFront.y;
    
    MinX = min(polyVoronoiTemp.x);
    tmpMsk = polyFrontTempX<MinX;
    if any(tmpMsk)
        OutMsk = tmpMsk & [tmpMsk(2:end); tmpMsk(1)] & [tmpMsk(end); tmpMsk(1:end-1)];
        polyFrontTempX = polyFrontTempX(~OutMsk);
        polyFrontTempY = polyFrontTempY(~OutMsk);
    end
        
    MaxX = max(polyVoronoiTemp.x);
    tmpMsk = polyFrontTempX>MaxX;
    if any(tmpMsk)
        OutMsk = tmpMsk & [tmpMsk(2:end); tmpMsk(1)] & [tmpMsk(end); tmpMsk(1:end-1)];
        polyFrontTempX = polyFrontTempX(~OutMsk);
        polyFrontTempY = polyFrontTempY(~OutMsk);
    end
    
    MinY = min(polyVoronoiTemp.y);
    tmpMsk = polyFrontTempY<MinY;
    if any(tmpMsk)
        OutMsk = tmpMsk & [tmpMsk(2:end); tmpMsk(1)] & [tmpMsk(end); tmpMsk(1:end-1)];
        polyFrontTempX = polyFrontTempX(~OutMsk);
        polyFrontTempY = polyFrontTempY(~OutMsk);
    end
    
    MaxY = max(polyVoronoiTemp.y);
    tmpMsk = polyFrontTempY>MaxY;
    if any(tmpMsk)
        OutMsk = tmpMsk & [tmpMsk(2:end); tmpMsk(1)] & [tmpMsk(end); tmpMsk(1:end-1)];
        polyFrontTempX = polyFrontTempX(~OutMsk);
        polyFrontTempY = polyFrontTempY(~OutMsk);
    end

    polyFrontTemp.x = polyFrontTempX;
    polyFrontTemp.y = polyFrontTempY;
    
% Clip 2 polygons     
    
    out = gpc_mexfile(polyVoronoiTemp,polyFrontTemp,1);
    
%     cla
%     patch(polyVoronoiTemp.x, polyVoronoiTemp.y, 'g')
%     hold on
%     plot(polyFront.x, polyFront.y, 'k.-')
%     patch(polyFrontTemp.x, polyFrontTemp.y, 'r')
%     patch(out.x, out.y, 'm')
%     hold off
    
%     if isempty(out)
%         for j = 1:10
%             polyVoronoiTemp.x = polyVoronoiTemp.x + int64(randi(10, size(polyVoronoiTemp.x)));
%             polyVoronoiTemp.y = polyVoronoiTemp.y + int64(randi(10, size(polyVoronoiTemp.x)));
%             out = clipper(polyVoronoiTemp,polyFrontTemp,1);
%             if ~isempty(out)
%                 break
%             end
%         end
%     end
    if length(out)>1
        LengthsIn = 0;
        AreaTemp = 0;
        VerticesCoordsTemp = [];
        for iPolygon = 1:length(out)
            VertCoordsX1 = (out(iPolygon).x)';
            VertCoordsY1 = (out(iPolygon).y)';
            
            M = size(VertCoordsX1,2);
            VertCoordsX2 = [VertCoordsX1(2:M) VertCoordsX1(1)];
            VertCoordsY2 = [VertCoordsY1(2:M) VertCoordsY1(1)];
            EdgeVectorsX = VertCoordsX1-VertCoordsX2;
            EdgeVectorsY = VertCoordsY1-VertCoordsY2;
            Lengths = sqrt(EdgeVectorsX.^2+EdgeVectorsY.^2);
            tmpMsk = Lengths>1e-9;
            VertCoordsX1 = VertCoordsX1(tmpMsk);
            VertCoordsY1 = VertCoordsY1(tmpMsk);
            VertCoordsX2 = VertCoordsX2(tmpMsk);
            VertCoordsY2 = VertCoordsY2(tmpMsk);
            EdgeVectorsX = EdgeVectorsX(tmpMsk);
            EdgeVectorsY = EdgeVectorsY(tmpMsk);
            Lengths = Lengths(tmpMsk);
            
            Lengths = ones(NumMidPts, 1)*Lengths;
            CrossProducts = VertCoordsX1.*VertCoordsY2 - VertCoordsY1.*VertCoordsX2;
            Midpoints2EdgesDistances =...
                (MidPointsTemp(:,1)*EdgeVectorsY-MidPointsTemp(:,2)*EdgeVectorsX...
                + ones(NumMidPts, 1)*CrossProducts)./Lengths;
            ScalarProducts = (DirectionsTemp(:,1)*EdgeVectorsX+DirectionsTemp(:,2)*EdgeVectorsY)./Lengths;
            
            Accept = abs(Midpoints2EdgesDistances)<Tolerance & abs(ScalarProducts)<1e-7;
            LengthsInTemp = zeros(NumMidPts, 1);
            Lengths = Lengths';
            LengthsInTemp(any(Accept')) = Lengths(Accept');
            
            LengthsIn = LengthsIn+LengthsInTemp;
            AreaTemp = AreaTemp+0.5*abs(sum(CrossProducts));
            if ~isempty(VertCoordsX1)
                VerticesCoordsTemp = [VerticesCoordsTemp [VertCoordsX1 VertCoordsX1(1); VertCoordsY1 VertCoordsY1(1)]];
            end
        end
        Area(i) = AreaTemp;
        S(Mask) = LengthsIn;
        VerticesCoords{i} = VerticesCoordsTemp;
    else
        VertCoordsX1 = (out.x)';
        VertCoordsY1 = (out.y)';
        M = size(VertCoordsX1,2);
        VertCoordsX2 = [VertCoordsX1(2:M) VertCoordsX1(1)];
        VertCoordsY2 = [VertCoordsY1(2:M) VertCoordsY1(1)];
        EdgeVectorsX = VertCoordsX1-VertCoordsX2;
        EdgeVectorsY = VertCoordsY1-VertCoordsY2;
        Lengths = sqrt(EdgeVectorsX.^2+EdgeVectorsY.^2);
        tmpMsk = Lengths>1e-9;
        VertCoordsX1 = VertCoordsX1(tmpMsk);
        VertCoordsY1 = VertCoordsY1(tmpMsk);
        VertCoordsX2 = VertCoordsX2(tmpMsk);
        VertCoordsY2 = VertCoordsY2(tmpMsk);
        EdgeVectorsX = EdgeVectorsX(tmpMsk);
        EdgeVectorsY = EdgeVectorsY(tmpMsk);
        Lengths = Lengths(tmpMsk);
        
        Lengths = ones(NumMidPts, 1)*Lengths;
        CrossProducts = VertCoordsX1.*VertCoordsY2 - VertCoordsY1.*VertCoordsX2;
        Midpoints2EdgesDistances =...
            (MidPointsTemp(:,1)*EdgeVectorsY-MidPointsTemp(:,2)*EdgeVectorsX...
            + ones(NumMidPts, 1)*CrossProducts)./Lengths;
        ScalarProducts = (DirectionsTemp(:,1)*EdgeVectorsX+DirectionsTemp(:,2)*EdgeVectorsY)./Lengths;
        
        Accept = abs(Midpoints2EdgesDistances)<Tolerance & abs(ScalarProducts)<1e-7;
%         LengthsIn = zeros(NumMidPts, 1);
        Lengths = Lengths';
%         LengthsIn(any(Accept')) = Lengths(Accept');
        LengthsIn = sum(Lengths'.*Accept,2);
        
        S(Mask) = LengthsIn;
        Area(i) = 0.5*abs(sum(CrossProducts));
        VerticesCoords{i} = [VertCoordsX1; VertCoordsY1];
    end
end