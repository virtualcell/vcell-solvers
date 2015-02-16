function VisualizeSolution(U, PosX, PosY, MeshSize, t, Front, doVelocitiField, showBoundaryVelocities)   

UniqueX = unique(PosX);
UniqueY = unique(PosY);
NumNodesX = length(UniqueX);
NumNodesY = length(UniqueY);

[InsideMask, InteriorMask, BoundaryMask] = IdentifyNodeTypes(PosX, PosY, Front, NumNodesX);
InsideIdx = find(InsideMask);
InteriorIdx = find(InteriorMask);
BoundaryIdx = find(BoundaryMask);

if ~doVelocitiField
    U_local = U(InsideMask);
    if showBoundaryVelocities
        vel = zeros(size(Front.Polygon));
        for i = 1:size(Front.Polygon,1)
            vel(i,:) = FT_velo_func(t, Front.Polygon(i,:));
        end
    end
    
    tri = DelaunayTri([PosX(InsideMask)   PosY(InsideMask)]);
    [v, c] = voronoiDiagram(tri);
    
    cla;
    hold on
    for j = 1:length(InteriorIdx)
        i = find(InsideIdx == InteriorIdx(j));
        if all(c{i}~=1)
            patch(v(c{i},1),v(c{i},2),U_local(i),'Edgecolor','none');
        end
    end
   
    [NatNeighborBoundaryPairs, Vertices, VoronoiCells] =...
        GetVoronoiDecomposition(BoundaryIdx, PosX, PosY, InsideMask, [], NumNodesX, NumNodesY);
    [~, Directions, MidPoints] = GetBoundaryIndependentMetrics(NatNeighborBoundaryPairs, PosX, PosY);
    [~, ~, VerticesCoords] =...
        GetBoundaryVolumes(BoundaryIdx, MeshSize, VoronoiCells, Vertices, NatNeighborBoundaryPairs, Directions, MidPoints, Front);
    
    for j = 1:length(BoundaryIdx)
        Idx = BoundaryIdx(j);
        patch(VerticesCoords{j}(1,:),VerticesCoords{j}(2,:),U_local(InsideIdx == Idx),'Edgecolor','none');
    end

    plot([Front.Polygon(:,1); Front.Polygon(1,1)],[Front.Polygon(:,2); Front.Polygon(1,2)],'k-','LineWidth',1)
    if showBoundaryVelocities
        quiver(Front.Polygon(:,1),Front.Polygon(:,2),vel(:,1),vel(:,2))
        title(['V_m_a_x  = ' num2str(max(sqrt(sum(vel.^2,2))), 3)],'Fontsize', 14);
    end
    % plot(PosX, PosY, 'k.')    
else
    Ux_local = U{1}(InsideMask);
    Uy_local = U{2}(InsideMask);
    plot([Front.Polygon(:,1); Front.Polygon(1,1)],[Front.Polygon(:,2); Front.Polygon(1,2)],'k-','LineWidth',1)
    hold on
    quiver(PosX(InsideMask), PosY(InsideMask), Ux_local, Uy_local, 2, 'color', 'k')
    hold off
end
xlim([min(UniqueX) max(UniqueX)])
ylim([min(UniqueY) max(UniqueY)])
xlabel('X','Fontsize', 14);
ylabel('Y','Fontsize', 14);

hold off
