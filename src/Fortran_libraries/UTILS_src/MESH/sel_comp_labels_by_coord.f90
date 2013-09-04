!>@file   sel_comp_labels_by_coord.f90
!!@brief  module sel_comp_labels_by_coord
!!
!!@author H. Matsui
!!@date Programmed in May, 2013
!
!>@brief Constants for geometry data
!!
!!@verbatim
!!      subroutine sel_coord_vector_comp_labels(iflag_coord,            &
!!     &          field_name, label_comp)
!1      subroutine sel_coord_tensor_comp_labels(iflag_coord,            &
!!     &          field_name, label_comp)
!!@endverbatim
!!
!!@n @param  iflag_coord  integer flag for coordinate system
!!
!!@n @param  field_name          input field name
!!@n @param  label_comp(3 or 6)  label for component
!
      module sel_comp_labels_by_coord
!
      use m_precision
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sel_coord_vector_comp_labels(iflag_coord,              &
     &          field_name, label_comp)
!
      use m_geometry_constants
      use add_direction_labels
!
      integer(kind = kint), intent(in) :: iflag_coord
      character(len=kchara), intent(in) :: field_name
      character(len=kchara), intent(inout) :: label_comp(3)
!
!
      if(iflag_coord .eq. iflag_spherical) then
        call add_vector_direction_label_rtp(field_name,                 &
     &             label_comp(1), label_comp(2), label_comp(3))
      else if(iflag_coord .eq. iflag_cylindrical) then
        call add_vector_direction_label_cyl(field_name,                 &
     &            label_comp(1), label_comp(2), label_comp(3))
      else
        call add_vector_direction_label_xyz(field_name,                 &
     &            label_comp(1), label_comp(2), label_comp(3))
      end if
!
      end subroutine sel_coord_vector_comp_labels
!
!-----------------------------------------------------------------------
!
      subroutine sel_coord_tensor_comp_labels(iflag_coord,              &
     &          field_name, label_comp)
!
      use m_geometry_constants
      use add_direction_labels
!
      integer(kind = kint), intent(in) :: iflag_coord
      character(len=kchara), intent(in) :: field_name
      character(len=kchara), intent(inout) :: label_comp(6)
!
!
      if(iflag_coord .eq. iflag_spherical) then
        call add_tensor_direction_label_rtp(field_name,                 &
     &             label_comp(1), label_comp(2), label_comp(3),         &
     &             label_comp(4), label_comp(5), label_comp(6) )
      else if(iflag_coord .eq. iflag_cylindrical) then
        call add_tensor_direction_label_cyl(field_name,                 &
     &             label_comp(1), label_comp(2), label_comp(3),         &
     &             label_comp(4), label_comp(5), label_comp(6) )
      else
        call add_tensor_direction_label_xyz(field_name,                 &
     &             label_comp(1), label_comp(2), label_comp(3),         &
     &             label_comp(4), label_comp(5), label_comp(6) )
      end if
!
      end subroutine sel_coord_tensor_comp_labels
!
!-----------------------------------------------------------------------
!
      end module sel_comp_labels_by_coord
