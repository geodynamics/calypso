!>@file   set_ucd_file_names.f90
!!@brief  module set_ucd_file_names
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui on June, 2006
!!@n       Modified by H.Matsui on March, 2013
!
!>@brief Append step, process, and file format suffix to UCD file prefix
!!@n      (If process number is negative, process number is not appeded)
!!
!!@verbatim
!!      subroutine delete_para_ucd_file(file_prefix, itype_file,        &
!!     &          nprocs, istep_ucd)
!!
!!      subroutine set_parallel_ucd_file_name(file_prefix, itype_file,  &
!!     &          my_rank, istep_ucd, file_name)
!!      subroutine set_parallel_grd_file_name(file_prefix, itype_file,  &
!!     &          my_rank, file_name)
!!
!!      subroutine set_single_ucd_file_name(file_prefix, itype_file,    &
!!     &          istep_ucd, file_name)
!!      subroutine set_single_grd_file_name(file_prefix, itype_file,    &
!!     &          file_name)
!!
!!      subroutine set_merged_hdf_mesh_file_name(file_prefix, file_name)
!!      subroutine set_merged_hdf_field_file_name(file_prefix,          &
!!      &         istep_ucd, file_name)
!!      subroutine set_merged_snap_xdmf_file_name(file_prefix,          &
!!     &          istep_ucd, file_name)
!!      subroutine set_merged_xdmf_file_name(file_prefix, file_name)
!!@endverbatim
!!
!!@param nprocs     number of subdomains
!!@param my_rank    subdomain ID
!!@param istep      Step number for VTK data
!
!
      module set_ucd_file_names
!
      use m_precision
!
      use m_constants
      use m_field_file_format
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine delete_para_ucd_file(file_prefix, itype_file,          &
     &          nprocs, istep_ucd)
!
      use delete_data_files
!
      character(len=kchara), intent(in) :: file_prefix
      integer(kind=kint), intent(in) :: itype_file, nprocs, istep_ucd
!
      integer(kind=kint) :: my_rank, ip
      character(len=kchara) :: file_name
!
!
      do ip = 1, nprocs
        my_rank = ip - 1
        call set_parallel_ucd_file_name(file_prefix,                    &
     &      itype_file, my_rank, istep_ucd, file_name)
!
        call delete_file_by_f(file_name)
      end do
!
      end subroutine delete_para_ucd_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine set_parallel_ucd_file_name(file_prefix, itype_file,    &
     &          my_rank, istep_ucd, file_name)
!
      use set_parallel_file_name
!
      integer(kind=kint), intent(in) :: itype_file, my_rank, istep_ucd
      character(len=kchara), intent(in) ::    file_prefix
      character(len=kchara), intent(inout) :: file_name
      character(len=kchara) :: fname_tmp
!
!
      call add_int_suffix(istep_ucd, file_prefix, fname_tmp)
!
      if (my_rank .ge. 0                                                &
     &      .and. (itype_file/100) .eq. (iflag_para/100)) then
        call add_int_suffix(my_rank, fname_tmp, file_name)
      else
        file_name = fname_tmp
      end if
!
      if(mod(itype_file,10) .eq. iflag_bin) then
        call add_flb_extension(file_name, fname_tmp)
        file_name = fname_tmp
        return
      else if(mod(itype_file,100)/10 .eq. iflag_vtk/10) then
        call add_vtk_extension(file_name, fname_tmp)
      else if(mod(itype_file,100)/10 .eq. iflag_vtd/10) then
        call add_vtd_extension(file_name, fname_tmp)
      else if(mod(itype_file,100)/10 .eq. iflag_ucd/10) then
        call add_ucd_extension(file_name, fname_tmp)
      else if(mod(itype_file,100)/10 .eq. iflag_udt/10) then
        call add_udt_extension(file_name, fname_tmp)
      else
        call add_fld_extension(file_name, fname_tmp)
      end if
!
      if (   mod(itype_file,10) .eq. iflag_gzip) then
        call add_gzip_extension(fname_tmp, file_name)
      else
        file_name = fname_tmp
      end if
!
      end subroutine set_parallel_ucd_file_name
!
!------------------------------------------------------------------
!
      subroutine set_parallel_grd_file_name(file_prefix, itype_file,    &
     &          my_rank, file_name)
!
      use set_parallel_file_name
!
      integer(kind=kint), intent(in) :: itype_file, my_rank
      character(len=kchara), intent(in) ::    file_prefix
      character(len=kchara), intent(inout) :: file_name
      character(len=kchara) :: fname_tmp
!
!
      call add_int_suffix(izero, file_prefix, fname_tmp)
!
      if (my_rank .ge. 0                                                &
     &     .and. itype_file/100 .eq. iflag_para/100) then
        call add_int_suffix(my_rank, fname_tmp, file_name)
      else
        file_name = fname_tmp
      end if
!
      if     (mod(itype_file,100)/10 .eq. iflag_vtd/10) then
        call add_vtg_extension(file_name, fname_tmp)
      else if(mod(itype_file,100)/10 .eq. iflag_udt/10) then
        call add_grd_extension(file_name, fname_tmp)
      else
        fname_tmp = file_name
      end if
!
      if (   mod(itype_file,10) .eq. iflag_gzip) then
        call add_gzip_extension(fname_tmp, file_name)
      else
        file_name = fname_tmp
      end if
!
      end subroutine set_parallel_grd_file_name
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine set_single_ucd_file_name(file_prefix, itype_file,      &
     &          istep_ucd, file_name)
!
      use set_parallel_file_name
!
      integer(kind=kint), intent(in) :: itype_file, istep_ucd
      character(len=kchara), intent(in) ::    file_prefix
      character(len=kchara), intent(inout) :: file_name
      character(len=kchara) :: fname_tmp
!
!
      call add_int_suffix(istep_ucd, file_prefix, file_name)
!
      if (    mod(itype_file,100)/10 .eq. iflag_vtk/10) then
        call add_vtk_extension(file_name, fname_tmp)
      else if(mod(itype_file,100)/10 .eq. iflag_vtd/10) then
        call add_vtd_extension(file_name, fname_tmp)
      else if(mod(itype_file,100)/10 .eq. iflag_ucd/10) then
        call add_ucd_extension(file_name, fname_tmp)
      else if(mod(itype_file,100)/10 .eq. iflag_udt/10) then
        call add_udt_extension(file_name, fname_tmp)
      else
        call add_fld_extension(file_name, fname_tmp)
      end if
!
      if (   mod(itype_file,10) .eq. iflag_gzip) then
        call add_gzip_extension(fname_tmp, file_name)
      else
        file_name = fname_tmp
      end if
!
      end subroutine set_single_ucd_file_name
!
!------------------------------------------------------------------
!
      subroutine set_single_grd_file_name(file_prefix, itype_file,      &
     &          file_name)
!
      use set_parallel_file_name
!
      integer(kind=kint), intent(in) :: itype_file
      character(len=kchara), intent(in) ::    file_prefix
      character(len=kchara), intent(inout) :: file_name
      character(len=kchara) :: fname_tmp
!
!
      call add_int_suffix(izero, file_prefix, file_name)
!
      if     (mod(itype_file,100)/10 .eq. iflag_vtd/10) then
        call add_vtg_extension(file_name, fname_tmp)
      else if(mod(itype_file,100)/10 .eq. iflag_udt/10) then
        call add_grd_extension(file_name, fname_tmp)
      else
        fname_tmp = file_name
      end if
!
      if (   mod(itype_file,10) .eq. iflag_gzip) then
        call add_gzip_extension(fname_tmp, file_name)
      else
        file_name = fname_tmp
      end if
!
      end subroutine set_single_grd_file_name
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine set_merged_hdf_mesh_file_name(file_prefix, file_name)
!
      use set_parallel_file_name
!
      character(len=kchara), intent(in) ::    file_prefix
      character(len=kchara), intent(inout) :: file_name
      character(len=kchara) :: fname_tmp
!
!
      call add_mesh_suffix(file_prefix, fname_tmp)
      fname_tmp = trim(fname_tmp)
      call add_hdf_extension(fname_tmp, file_name)
!
      end subroutine set_merged_hdf_mesh_file_name
!
!------------------------------------------------------------------
!
      subroutine set_merged_hdf_field_file_name(file_prefix,            &
      &         istep_ucd, file_name)
!
      use set_parallel_file_name
!
      character(len=kchara), intent(in) ::    file_prefix
      integer(kind=kint), intent(in) :: istep_ucd
      character(len=kchara), intent(inout) :: file_name
      character(len=kchara) :: fname_tmp, fname_tmp2
!
!
      call add_field_suffix(file_prefix, fname_tmp)
      fname_tmp = trim(fname_tmp)
      call add_int_suffix(istep_ucd, fname_tmp, fname_tmp2)
      fname_tmp2 = trim(fname_tmp2)
      call add_hdf_extension(fname_tmp2, file_name)
!
      end subroutine set_merged_hdf_field_file_name
!
!------------------------------------------------------------------
!
      subroutine set_merged_snap_xdmf_file_name(file_prefix,            &
     &          istep_ucd, file_name)
!
      use set_parallel_file_name
!
      character(len=kchara), intent(in) ::    file_prefix
      integer(kind=kint), intent(in) :: istep_ucd
      character(len=kchara), intent(inout) :: file_name
      character(len=kchara) :: fname_tmp
!
!
      call add_int_suffix(istep_ucd, file_prefix, fname_tmp)
      fname_tmp = trim(fname_tmp)
      call add_xdmf_extension(fname_tmp, file_name)
!
      end subroutine set_merged_snap_xdmf_file_name
!
!------------------------------------------------------------------
!
      subroutine set_merged_xdmf_file_name(file_prefix, file_name)
!
      use set_parallel_file_name
!
      character(len=kchara), intent(in) ::    file_prefix
      character(len=kchara), intent(inout) :: file_name
      character(len=kchara) :: fname_tmp
!
!
      call add_xdmf_suffix(file_prefix, fname_tmp)
      fname_tmp = trim(fname_tmp)
      call add_xdmf_extension(fname_tmp, file_name)
!
      end subroutine set_merged_xdmf_file_name
!
!------------------------------------------------------------------
!
      end module set_ucd_file_names
