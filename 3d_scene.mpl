with(plots):
with(plottools):

Vertex := module()
  option object;

  export x, y, z;

  export ModuleApply :: static :=
  proc(x::numeric, y::numeric, z::numeric)
    local self;
    self := Object(Vertex);
    self:-x := x;
    self:-y := y;
    self:-z := z;
    return self;
  end proc;

  export Copy :=
  proc()
    return Vertex(x, y, z);
  end proc;

  export GetCoords :=
  proc()
    return [x, y, z];
  end proc;

  export SetCoords :=
  proc(new_x::numeric, new_y::numeric, new_z::numeric)
    x := new_x;
    y := new_y;
    z := new_z;
  end proc;

  export Translate :=
  proc(dx::numeric, dy::numeric, dz::numeric)
    x := x + dx;
    y := y + dy;
    z := z + dz;
  end proc;

  export Rotate :=
  proc(rx::numeric, ry::numeric, rz::numeric)
    local rx_deg, ry_deg, rz_deg;
    local cosX, sinX,
          cosY, sinY,
          cosZ, sinZ;
    local new_x, new_y, new_z;

    rx_deg := rx * Pi / 180;
    ry_deg := ry * Pi / 180;
    rz_deg := rz * Pi / 180;

    cosX := cos(rx_deg); sinX := sin(rx_deg);
    cosY := cos(ry_deg); sinY := sin(ry_deg);
    cosZ := cos(rz_deg); sinZ := sin(rz_deg);

    new_x := x*(cosY*cosZ) + y*(cosZ*sinX*sinY - cosX*sinZ) + z*(sinX*sinZ + cosX*cosZ*sinY);
    new_y := x*(cosY*sinZ) + y*(cosX*cosZ + sinX*sinY*sinZ) + z*(cosX*sinY*sinZ - cosZ*sinX);
    new_z := x*(-sinY) + y*(cosY*sinX) + z*(cosX*cosY);

    x := evalf(new_x);
    y := evalf(new_y);
    z := evalf(new_z);

  end proc;

  export Scale :=
  proc(sx::numeric, sy::numeric, sz::numeric)
    if sx = 0 or sy = 0 or sz = 0 then
      error "Cannot scale because value is zero"
    else
      x := x * sx;
      y := y * sy;
      z := z * sz;
    end if;
  end proc;

  export ScaleU :=
  proc(value::numeric)
    if value = 0 then
      error "Cannot scale because value is zero"
    else
      x := x * value;
      y := y * value;
      z := z * value;
    end if;
  end proc;

end module;

Edge := module()
  option object;

  export vertex_one, vertex_two;

  export ModuleApply :: static :=
  proc(vertex_one::Vertex, vertex_two::Vertex)
    local self;
    self := Object(Edge);
    self:-vertex_one := vertex_one;
    self:-vertex_two := vertex_two;
    return self;
  end proc;

  export Copy :=
  proc()
    return Edge(vertex_one:-Copy(), vertex_two:-Copy());
  end proc;

  export GetVertexCoords :=
  proc()
    return [vertex_one:-GetCoords(),
            vertex_two:-GetCoords()];
  end proc;

  export Translate :=
  proc(dx::numeric, dy::numeric, dz::numeric)
    vertex_one:-Translate(dx, dy, dz);
    vertex_two:-Translate(dx, dy, dz);
  end proc;

  export Rotate :=
  proc(rx::numeric, ry::numeric, rz::numeric)
    vertex_one:-Rotate(rx, ry, rz);
    vertex_two:-Rotate(rx, ry, rz);
  end proc;

end module;

Face := module()
  option object;

  export vertices;

  export ModuleApply :: static :=
  proc(vertices::list)
    local self;
    self:= Object(Face);
    self:-vertices := vertices;
    return self;
  end proc;

  export Copy :=
  proc()
    local i, copied_vertices;
    copied_vertices := [];
    for i from 1 to nops(vertices) do
      copied_vertices := [op(copied_vertices), vertices[i]:-Copy()];
    end do;
    return Face(copied_vertices);
  end proc;

  export GetVertexCoords :=
  proc()
    local vertex_coodrs, i;
    vertex_coodrs := [];
    for i from 1 to nops(vertices) do
      vertex_coodrs := [op(vertex_coodrs), vertices[i]:-GetCoords()];
    end do;
    return  vertex_coodrs;
  end proc;

  export Translate :=
  proc(dx::numeric, dy::numeric, dz::numeric)
    local i;
    for i from 1 to nops(vertices) do
      vertices[i]:-Translate(dx, dy, dz);
    end do;
  end proc;

  export Rotate :=
  proc(rx::numeric, ry::numeric, rz::numeric)
    local i;
    for i from 1 to nops(vertices) do
      vertices[i]:-Rotate(rx, ry, rz);
    end do;
  end proc;

  export Scale :=
  proc(sx::numeric, sy::numeric, sz::numeric)
    local i;
    for i from 1 to nops(vertices) do
      vertices[i]:-Scale(sx, sy, sz);
    end do;
  end proc;


  export ScaleU :=
  proc(value::numeric)
    local i;
    for i from 1 to nops(vertices) do
      vertices[i]:-ScaleU(value);
    end do;
  end proc;

end module;

Mesh := module()
  option object;

  export vertices, edges, faces;

  export  ModuleApply :: static :=
  proc(vertices::list, faces::list)
    local self;
    self := Object(Mesh);
    self:-vertices := vertices;
    self:-faces := faces;
    return self;
  end proc;

  export Copy :=
  proc()
    local i, copied_vertices, copied_faces, vertex_map, old_face, new_face_vertices, j;

    copied_vertices := [];
    vertex_map := table();

    for i from 1 to nops(vertices) do
      copied_vertices := [op(copied_vertices), vertices[i]:-Copy()];
      vertex_map[vertices[i]] := copied_vertices[i];
    end do;

    copied_faces := [];
    for i from 1 to nops(faces) do
      old_face := faces[i];
      new_face_vertices := [];

      for j from 1 to nops(old_face:-vertices) do
        new_face_vertices := [op(new_face_vertices), vertex_map[old_face:-vertices[j]]];
      end do;
      copied_faces := [op(copied_faces), Face(new_face_vertices)];
    end do;

    return Mesh(copied_vertices, copied_faces);
  end proc;

  export CreateSurface :=
  proc(color::string)
    local surface_parts, i;
    surface_parts := [];
    for i from 1 to nops(faces) do
      surface_parts := [op(surface_parts), polygon(faces[i]:-GetVertexCoords(), color=color)];
    end do;
    return surface_parts;
  end proc;

  export Display :=
  proc(surface_color::string := "green")
    local surface;
    surface := CreateSurface(surface_color);
    plots:-display(surface, _rest);
  end proc;

  export Translate :=
  proc(dx::numeric, dy::numeric, dz::numeric)
    local i;
    for i from 1 to nops(vertices) do
      vertices[i]:-Translate(dx, dy, dz);
    end do;
  end proc;

  export Rotate :=
  proc(rx::numeric, ry::numeric, rz::numeric)
    local i;
    for i from 1 to nops(vertices) do
      vertices[i]:-Rotate(rx, ry, rz);
    end do;
  end proc;

  export Scale :=
  proc(sx::numeric, sy::numeric, sz::numeric)
    local i;
    for i from 1 to nops(vertices) do
      vertices[i]:-Scale(sx, sy, sz);
    end do;
  end proc;

  export ScaleU :=
  proc(value::numeric)
    local i;
    for i from 1 to nops(vertices) do
      vertices[i]:-ScaleU(value);
    end do;
  end proc;

end module;

PrimitiveFactory := module()
  option object;

  export CreateCube :=
  proc(center::list := [0, 0, 0], size::numeric := 1)
    local half_size, vertices, faces;
    half_size := size / 2;
    vertices := [
      Vertex(center[1] - half_size, center[2] - half_size, center[3] - half_size),
      Vertex(center[1] + half_size, center[2] - half_size, center[3] - half_size),
      Vertex(center[1] + half_size, center[2] + half_size, center[3] - half_size),
      Vertex(center[1] - half_size, center[2] + half_size, center[3] - half_size),
      Vertex(center[1] - half_size, center[2] - half_size, center[3] + half_size),
      Vertex(center[1] + half_size, center[2] - half_size, center[3] + half_size),
      Vertex(center[1] + half_size, center[2] + half_size, center[3] + half_size),
      Vertex(center[1] - half_size, center[2] + half_size, center[3] + half_size)
    ];

    faces := [
      Face([vertices[1], vertices[2], vertices[3], vertices[4]]), # Задняя грань
      Face([vertices[5], vertices[6], vertices[7], vertices[8]]), # Передняя грань
      Face([vertices[1], vertices[2], vertices[6], vertices[5]]), # Нижняя грань
      Face([vertices[3], vertices[4], vertices[8], vertices[7]]), # Верхняя грань
      Face([vertices[2], vertices[3], vertices[7], vertices[6]]), # Правая грань
      Face([vertices[1], vertices[4], vertices[8], vertices[5]])  # Левая грань
    ];

    return Mesh(vertices, faces);
  end proc;

  export CreateCylinder :=
  proc(center::list := [0, 0, 0], radius::numeric := 0.5, height::numeric := 1, segments::integer := 12)
    local i, angle, vertices, faces, top_vertices, bottom_vertices;
    local x, y, z_top, z_bottom, next_i;

    vertices := [];
    faces := [];
    top_vertices := [];
    bottom_vertices := [];

    z_top := center[3] + height/2;
    z_bottom := center[3] - height/2;

    for i from 0 to segments-1 do
      angle := 2 * Pi * i / segments;
      x := evalf(center[1] + radius * cos(angle));
      y := evalf(center[2] + radius * sin(angle));

      top_vertices := [op(top_vertices), Vertex(x, y, z_top)];
      bottom_vertices := [op(bottom_vertices), Vertex(x, y, z_bottom)];
    end do;

    vertices := [op(top_vertices), op(bottom_vertices)];
    faces := [op(faces), Face(top_vertices)];
    faces := [op(faces), Face(bottom_vertices)];

    for i from 1 to segments do
      next_i := i mod segments + 1;
      faces := [op(faces), Face([
          top_vertices[i],
          top_vertices[next_i],
          bottom_vertices[next_i],
          bottom_vertices[i]
        ])];
    end do;

    return Mesh(vertices, faces);
  end proc;

  export CreateCircle :=
  proc(center::list := [0, 0, 0], radius::numeric := 1, segments::integer := 16, plane::string := "xy")
    local i, angle, vertices, face, x, y, z;

    vertices := [];

    for i from 0 to segments-1 do
      angle := 2 * Pi * i / segments;

      if plane = "xy" then
        x := center[1] + radius * cos(angle);
        y := center[2] + radius * sin(angle);
        z := center[3];
      elif plane = "xz" then
        x := center[1] + radius * cos(angle);
        y := center[2];
        z := center[3] + radius * sin(angle);
      elif plane = "yz" then
        x := center[1];
        y := center[2] + radius * cos(angle);
        z := center[3] + radius * sin(angle);
      else
        error "Plane must be 'xy', 'xz', or 'yz'";
      end if;

      vertices := [op(vertices), Vertex(evalf(x), evalf(y), evalf(z))];
    end do;

    face := Face(vertices);
    return Mesh(vertices, [face]);
  end proc;

end module;

Modifiers := module()
  option object;

  export Array :=
  proc(obj::Mesh, count::posint,
       dx::numeric, dy::numeric, dz::numeric)
    local i, meshes, current_mesh;

    if count < 1 then
      error "Count must be positive integer";
    end if;

    if dx = 0 and dy = 0 and dz = 0 then
      error "Offset cannot be zero in all directions";
    end if;

    meshes := [obj];

    for i from 1 to count do
      current_mesh := obj:-Copy();
      current_mesh:-Translate(dx*i, dy*i, dz*i);
      meshes := [op(meshes), current_mesh];
    end do;

    return meshes;
  end proc;

end module;

Scene := module()
  option object;

  export objects;

  export ModuleApply :: static :=
  proc(objs::list)
    local self;
    self := Object(Scene);
    self:-objects := objs;
    return self;
  end proc;

  export AddObject :=
  proc(obj::Mesh)
    objects := [op(objects), obj];
  end proc;

  export AddObjects :=
  proc(objs::list)
    objects := [op(objects), op(objs)];
  end proc;

  export Display :=
  proc(surface_color::string := "green")
    local i, all_surfaces;
    all_surfaces := [];
    for i from 1 to nops(objects) do
      all_surfaces := [op(all_surfaces), op(objects[i]:-CreateSurface(surface_color))];
    end do;
    plots:-display(all_surfaces, scaling=constrained);
  end proc;

end module;

scene := Scene([]):
scene:-AddObject(PrimitiveFactory:-CreateCircle([0,0,0], 1, 5));
cy := PrimitiveFactory:-CreateCylinder();
cy:-Rotate(0, 40, 0);
scene:-AddObject(cy);
scene:-Display();