!>@file   set_nnod_4_ele_by_type.f90
!!@brief  module set_nnod_4_ele_by_type
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2006
!
!>@brief  Set Number of element from element type ID
!!
!!@verbatim
!!      subroutine set_3D_nnod_4_ele_by_type(itype,                     &
!!     &          nnod_4_ele, nnod_4_surf, nnod_4_edge)
!!      integer(kind = kint) function set_nnod_4_ele_by_eletype(itype)
!!      subroutine set_3D_nnod_4_sfed_by_ele                            &
!!     &         (nnod_4_ele, nnod_4_surf, nnod_4_edge)
!!      subroutine set_3D_nnod_4_sfed_by_ele,                           &
!!     &         (nnod_4_ele, nnod_4_surf, nnod_4_edge)
!!      subroutine s_set_nnod_4_ele_by_type(itype, nnod_4_ele)
!!
!!      integer(kind = kint) function                                   &
!!     &              set_cube_eletype_from_num(nnod_4_ele)
!!@endverbatim
!
      module set_nnod_4_ele_by_type
!
      use m_precision
!
      implicit  none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine set_3D_nnod_4_ele_by_type(itype,                       &
     &          nnod_4_ele, nnod_4_surf, nnod_4_edge)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: itype
      integer(kind = kint), intent(inout) :: nnod_4_ele
      integer(kind = kint), intent(inout) :: nnod_4_surf
      integer(kind = kint), intent(inout) :: nnod_4_edge
!
!
      nnod_4_ele = set_nnod_4_ele_by_eletype(itype)
      call set_3D_nnod_4_sfed_by_ele                                    &
     &   (nnod_4_ele, nnod_4_surf, nnod_4_edge)
!
      end subroutine set_3D_nnod_4_ele_by_type
!
!------------------------------------------------------------------
!
      integer(kind = kint) function set_nnod_4_ele_by_eletype(itype)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: itype
!
!
      if (itype .eq. 332) then
        set_nnod_4_ele_by_eletype =  num_t_quad
      else if (itype .eq. 333) then
        set_nnod_4_ele_by_eletype =  num_t_lag
      else if (itype .eq. 331) then
        set_nnod_4_ele_by_eletype =  num_t_linear
      else
        set_nnod_4_ele_by_eletype =  num_t_linear
      end if
!
      end function set_nnod_4_ele_by_eletype
!
!------------------------------------------------------------------
!
      subroutine set_3D_nnod_4_sfed_by_ele                              &
     &         (nnod_4_ele, nnod_4_surf, nnod_4_edge)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: nnod_4_ele
      integer(kind = kint), intent(inout) :: nnod_4_surf
      integer(kind = kint), intent(inout) :: nnod_4_edge
!
!
      if (nnod_4_ele .eq. num_t_quad) then
        nnod_4_surf = num_quad_sf
        nnod_4_edge = num_quad_edge
      else if (nnod_4_ele .eq. num_t_linear) then
        nnod_4_surf = num_linear_sf
        nnod_4_edge = num_linear_edge
      else if (nnod_4_ele .eq. num_t_lag) then
        nnod_4_surf = num_lag_sf
        nnod_4_edge = num_quad_edge
      end if
!
      end subroutine set_3D_nnod_4_sfed_by_ele
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine s_set_nnod_4_ele_by_type(itype, nnod_4_ele)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: itype
      integer(kind = kint), intent(inout) :: nnod_4_ele
!
!
      if      (itype.eq.111) then
        nnod_4_ele =  num_linear_edge
      else if (itype.eq.112) then
        nnod_4_ele =  num_quad_edge
      else if (itype.eq.211) then
        nnod_4_ele =  num_quad_edge
      else if (itype.eq.212) then
        nnod_4_ele =  6
      else if (itype.eq.221) then
        nnod_4_ele =  num_linear_sf
      else if (itype.eq.222) then
        nnod_4_ele =  num_quad_sf
      else if (itype.eq.223) then
        nnod_4_ele =  num_lag_sf
      else if (itype.eq.311) then
        nnod_4_ele =  4
      else if (itype.eq.312) then
        nnod_4_ele = 10
      else if (itype.eq.321) then
        nnod_4_ele =  6
      else if (itype.eq.322) then
        nnod_4_ele = 15
      else if (itype.eq.331) then
        nnod_4_ele = num_t_linear
      else if (itype.eq.332) then
        nnod_4_ele = num_t_quad
      else if (itype.eq.333) then
        nnod_4_ele = num_t_lag
      else if (itype.eq.411) then
        nnod_4_ele =  4
      else if (itype.eq.412) then
        nnod_4_ele =  7
      else if (itype.eq.421) then
        nnod_4_ele =  5
      else if (itype.eq.422) then
        nnod_4_ele =  9
      else if (itype.eq.511) then
        nnod_4_ele =  6
      else if (itype.eq.512) then
        nnod_4_ele = 12
      else if (itype.eq.521) then
        nnod_4_ele =  8
      else if (itype.eq.522) then
        nnod_4_ele = 16
      else if (itype.eq.611) then
        nnod_4_ele =  2
      else if (itype.eq.612) then
        nnod_4_ele =  3
      else if (itype.eq.711) then
        nnod_4_ele =  3
      else if (itype.eq.712) then
        nnod_4_ele =  6
      else if (itype.eq.721) then
        nnod_4_ele =  4
      else if (itype.eq.722) then
        nnod_4_ele =  8
      end if
!
      end subroutine s_set_nnod_4_ele_by_type
!
!------------------------------------------------------------------
!
      integer(kind = kint) function                                     &
     &              set_cube_eletype_from_num(nnod_4_ele)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: nnod_4_ele
!
!
      if (nnod_4_ele .eq. num_t_quad) then
        set_cube_eletype_from_num =  332
      else if (nnod_4_ele .eq. num_t_lag) then
        set_cube_eletype_from_num =  333
      else if (nnod_4_ele .eq. num_t_linear) then
        set_cube_eletype_from_num =  331
      else
        set_cube_eletype_from_num =  331
      end if
!
      end function set_cube_eletype_from_num
!
!------------------------------------------------------------------
!
      end module set_nnod_4_ele_by_type
