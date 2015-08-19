!>@file   output_4_psf.f90
!!@brief  module output_4_psf
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!!@n    modified in July, 2014
!
!>@brief Output routines for surfacing module
!!
!!@verbatim
!!      subroutine output_section_mesh(num_psf, psf_header,             &
!!     &          itype_psf_file, psf_mesh, psf_out, psf_out_m)
!!      subroutine output_section_data                                  &
!!     &         (num_psf, istep_psf, psf_mesh, psf_out, psf_out_m)
!!
!!      subroutine output_isosurface                                    &
!!     &         (num_iso, iso_header, itype_iso_file, istep_iso,       &
!!     &         iso_mesh, iso_out, iso_out_m)
!!@endverbatim
!
      module output_4_psf
!
      use m_precision
!
      use calypso_mpi
      use t_ucd_data
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine output_section_mesh(num_psf, psf_header,               &
     &          itype_psf_file, psf_mesh, psf_out, psf_out_m)
!
      use t_psf_patch_data
      use set_ucd_data_to_type
      use parallel_ucd_IO_select
      use ucd_IO_select
      use merge_output_4_psf
!
      integer(kind= kint), intent(in) :: num_psf
      character(len = kchara), intent(in) :: psf_header(num_psf)
      integer(kind= kint), intent(in) :: itype_psf_file(num_psf)
      type(psf_local_data), intent(in) :: psf_mesh(num_psf)
      type(ucd_data), intent(inout) ::        psf_out(num_psf)
      type(merged_ucd_data), intent(inout) :: psf_out_m(num_psf)
!
      integer(kind= kint) :: i_psf, irank_tgt
!
!
      do i_psf = 1, num_psf
        psf_out(i_psf)%ifmt_file = itype_psf_file(i_psf)
        if((psf_out(i_psf)%ifmt_file/iflag_single) .eq. 0) then
          irank_tgt = mod(i_psf-1,nprocs)
          call merge_ucd_psf_mesh                                       &
     &       (irank_tgt, psf_mesh(i_psf), psf_out(i_psf))
          call calypso_mpi_barrier
        else
          call link_nnod_stacks_type_2_output(nprocs,                   &
     &          psf_mesh(i_psf)%node, psf_mesh(i_psf)%patch,            &
     &          psf_out_m(i_psf))
!
          call link_node_data_type_2_output                             &
     &         (psf_mesh(i_psf)%node, psf_out(i_psf))
          call link_ele_data_type_2_output                              &
     &         (psf_mesh(i_psf)%patch, psf_out(i_psf))
          call link_field_data_type_2_output                            &
     &       (psf_mesh(i_psf)%node%numnod, psf_mesh(i_psf)%field,       &
     &        psf_out(i_psf))
        end if
      end do
!
      do i_psf = 1, num_psf
        psf_out(i_psf)%file_prefix = psf_header(i_psf)
        if((psf_out(i_psf)%ifmt_file/iflag_single) .eq. 0) then
          if(my_rank .eq. mod(i_psf-1,nprocs)) then
            call sel_write_grd_file(iminus, psf_out(i_psf))
          end if
        else
          call sel_write_parallel_ucd_mesh                              &
     &       (psf_out(i_psf), psf_out_m(i_psf))
        end if
      end do
      call calypso_mpi_barrier
!
      end subroutine output_section_mesh
!
!  ---------------------------------------------------------------------
!
      subroutine output_section_data                                    &
     &         (num_psf, istep_psf, psf_mesh, psf_out, psf_out_m)
!
      use set_ucd_data_to_type
      use parallel_ucd_IO_select
      use ucd_IO_select
      use merge_output_4_psf
!
      integer(kind= kint), intent(in) :: num_psf
      integer(kind= kint), intent(in) ::  istep_psf
      type(psf_local_data), intent(in) :: psf_mesh(num_psf)
      type(ucd_data), intent(inout) ::        psf_out(num_psf)
      type(merged_ucd_data), intent(inout) :: psf_out_m(num_psf)
!
      integer(kind= kint) :: i_psf, irank_tgt
!
!
      do i_psf = 1, num_psf
        if((psf_out(i_psf)%ifmt_file/iflag_single) .eq. 0) then
          irank_tgt = mod(i_psf-1,nprocs)
          call merge_ucd_psf_data                                       &
     &       (irank_tgt, psf_mesh(i_psf), psf_out(i_psf))
        end if
      end do
!
      do i_psf = 1, num_psf
        if((psf_out(i_psf)%ifmt_file/iflag_single) .eq. 0) then
          if(my_rank .eq. mod(i_psf-1,nprocs)) then
            call sel_write_udt_file(iminus, istep_psf, psf_out(i_psf))
          end if
        else
          call sel_write_parallel_ucd_file                              &
     &      (istep_psf, psf_out(i_psf), psf_out_m(i_psf))
        end if
      end do
      call calypso_mpi_barrier
!
      end subroutine output_section_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine output_isosurface                                      &
     &         (num_iso, iso_header, itype_iso_file, istep_iso,         &
     &         iso_mesh, iso_out, iso_out_m)
!
      use t_psf_patch_data
      use set_ucd_data_to_type
      use parallel_ucd_IO_select
      use ucd_IO_select
      use merge_output_4_psf
!
      integer(kind= kint), intent(in) :: num_iso
      character(len = kchara), intent(in) :: iso_header(num_iso)
      integer(kind= kint), intent(in) :: itype_iso_file(num_iso)
      integer(kind= kint), intent(in) :: istep_iso
      type(psf_local_data), intent(in) :: iso_mesh(num_iso)
!
!>      Structure for isosurface output (used by master process)
      type(ucd_data), intent(inout) :: iso_out(num_iso)
      type(merged_ucd_data), intent(inout) :: iso_out_m(num_iso)
!
      integer(kind= kint) :: i_iso, irank_tgt
!
!
      do i_iso = 1, num_iso
        iso_out(i_iso)%ifmt_file = itype_iso_file(i_iso)
        if((iso_out(i_iso)%ifmt_file/iflag_single) .eq. 0) then
          irank_tgt = mod(i_iso-1,nprocs)
          call merge_ucd_psf_mesh                                       &
     &       (irank_tgt, iso_mesh(i_iso), iso_out(i_iso))
          call merge_ucd_psf_data                                       &
     &       (irank_tgt, iso_mesh(i_iso), iso_out(i_iso))
        else
          call link_nnod_stacks_type_2_output(nprocs,                   &
     &        iso_mesh(i_iso)%node, iso_mesh(i_iso)%patch,              &
     &        iso_out_m(i_iso))
!
          call link_node_data_type_2_output                             &
     &       (iso_mesh(i_iso)%node, iso_out(i_iso))
          call link_ele_data_type_2_output                              &
     &       (iso_mesh(i_iso)%patch, iso_out(i_iso))
          call link_field_data_type_2_output                            &
     &       (iso_mesh(i_iso)%node%numnod,                              &
     &        iso_mesh(i_iso)%field, iso_out(i_iso))
!
        end if
      end do
!
      do i_iso = 1, num_iso
        iso_out(i_iso)%file_prefix = iso_header(i_iso)
        if((iso_out(i_iso)%ifmt_file/iflag_single) .eq. 0) then
          if(my_rank .eq. mod(i_iso-1,nprocs)) then
            call sel_write_ucd_file(iminus, istep_iso, iso_out(i_iso))
          end if
          call deallocate_ucd_mesh(iso_out(i_iso))
        else
          call sel_write_parallel_ucd_file                              &
     &       (istep_iso, iso_out(i_iso), iso_out_m(i_iso))
          call disconnect_merged_ucd_mesh                               &
     &       (iso_out(i_iso), iso_out_m(i_iso))
        end if
      end do
!
      call calypso_mpi_barrier
!
      end subroutine output_isosurface
!
!  ---------------------------------------------------------------------
!
      end module output_4_psf
