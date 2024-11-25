!>@file   set_fline_seed_from_tracer.f90
!!@brief  module set_fline_seed_from_tracer
!!
!!@date  Programmed by H.Matsui in Aug. 2011
!
!>@brief control parameters for each field line
!!
!!@verbatim
!!      subroutine const_fline_seed_from_tracer(node, ele, nod_fld,     &
!!     &          num_tracer, tracer_tce, fln_prm, fln_tce)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_data), intent(in) :: nod_fld
!!        type(fieldline_paramter), intent(in) :: fln_prm
!!        integer(kind = kint), intent(in) :: num_tracer
!!      type(each_fieldline_trace), intent(in)                          &
!!     &                           :: tracer_tce(num_tracer)
!!        type(each_fieldline_trace), intent(inout) :: fln_tce
!!@endverbatim
!
      module set_fline_seed_from_tracer
!
      use m_precision
      use m_geometry_constants
      use t_geometry_data
      use t_phys_data
      use t_file_IO_parameter
      use t_control_params_4_fline
      use t_tracing_data
      use t_ctl_params_viz_fields
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine const_fline_seed_from_tracer(node, ele, nod_fld,       &
     &          num_tracer, tracer_tce, fln_prm, fln_tce)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(fieldline_paramter), intent(in) :: fln_prm
      integer(kind = kint), intent(in) :: num_tracer
      type(each_fieldline_trace), intent(in)                            &
     &                           :: tracer_tce(num_tracer)
!
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
!
      fln_tce%num_current_fline = count_fline_seed_from_tracer          &
     &                (tracer_tce(fln_prm%id_tracer_for_seed), fln_prm)
      call resize_line_start_fline(fln_tce%num_current_fline,           &
     &                             fln_prm%fline_fields, fln_tce)
      call s_set_fline_seed_from_tracer(node, ele, nod_fld,             &
     &    tracer_tce(fln_prm%id_tracer_for_seed), fln_prm, fln_tce)
!
      end subroutine const_fline_seed_from_tracer
!
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function count_fline_seed_from_tracer        &
     &                            (org_fln_tce, fln_prm)
!
      type(fieldline_paramter), intent(in) :: fln_prm
      type(each_fieldline_trace), intent(in) :: org_fln_tce
!
      integer(kind = kint) :: num
!
!
      num = org_fln_tce%num_current_fline
      if(fln_prm%id_fline_direction .eq. iflag_both_trace) then
        num = 2 * num
      end if
      count_fline_seed_from_tracer = num
!
      end function count_fline_seed_from_tracer
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_fline_seed_from_tracer(node, ele, nod_fld,       &
     &          org_fln_tce, fln_prm, fln_tce)
!
      use set_fline_seeds_from_list
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(fieldline_paramter), intent(in) :: fln_prm
      type(each_fieldline_trace), intent(in) :: org_fln_tce
!
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
      integer(kind= kint) :: inum, icou
!
!
      icou = 0
      do inum = 1, fln_tce%num_current_fline
        icou = icou + 1
        fln_tce%iflag_direction(icou) = fln_prm%id_fline_direction
        fln_tce%iline_original(icou)                                    &
     &             = org_fln_tce%iline_original(inum)
        fln_tce%isf_dbl_start(1:3,icou)                                 &
     &             = org_fln_tce%isf_dbl_start(1:3,inum)
        fln_tce%xx_fline_start(1:4,icou)                                &
     &             = org_fln_tce%xx_fline_start(1:4,icou)
        call set_field_at_each_seed_point(node, ele, nod_fld,           &
     &      fln_prm%fline_fields, fln_prm%iphys_4_fline,                &
     &      fln_tce%isf_dbl_start(2,inum),                              &
     &      fln_tce%xx_fline_start(1,inum),                             &
     &      fln_tce%v_fline_start(1,inum),                              &
     &      fln_tce%c_fline_start(1,inum))
!
        fln_tce%trace_length(icou) = 0.0d0
        fln_tce%icount_fline(icou) = 0
!
        if(fln_prm%id_fline_direction .eq. iflag_both_trace) then
          fln_tce%iflag_direction(icou) = fln_prm%id_fline_direction
!
          icou = icou + 1
          fln_tce%iline_original(icou) = fln_tce%iline_original(icou-1)
          fln_tce%isf_dbl_start(1:3,icou)                               &
     &             = org_fln_tce%isf_dbl_start(1:3,icou-1)
          call copy_global_start_fline(icou, (icou-1),                  &
     &                                 fln_prm%fline_fields, fln_tce)
!
          fln_tce%trace_length(icou) = 0.0d0
          fln_tce%icount_fline(icou) = 0
        end if
      end do
!
      end subroutine s_set_fline_seed_from_tracer
!
!  ---------------------------------------------------------------------
!
      end module set_fline_seed_from_tracer
