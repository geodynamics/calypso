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
!!      character(len=kchara) function set_parallel_ucd_file_name       &
!!     &                  (file_prefix, itype_file, id_rank, istep_ucd)
!!      character(len=kchara) function set_parallel_grd_file_name       &
!!     &                    (file_prefix, itype_file, id_rank)
!!
!!      character(len=kchara) function set_single_ucd_file_name         &
!!     &                    (file_prefix, itype_file, istep_ucd)
!!      character(len=kchara) function                                  &
!!     &          set_single_grd_file_name(file_prefix, itype_file)
!!
!!      character(len=kchara) function                                  &
!!     &          set_merged_hdf_mesh_file_name(file_prefix)
!!      character(len=kchara) function                                  &
!!     &         set_merged_hdf_field_file_name(file_prefix, istep_ucd)
!!      character(len=kchara) function                                  &
!!     &          set_merged_snap_xdmf_file_name(file_prefix, istep_ucd)
!!      character(len=kchara) function                                  &
!!     &                     set_merged_xdmf_file_name(file_prefix)
!!@endverbatim
!!
!!@param nprocs     number of subdomains
!!@param id_rank    subdomain ID
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
      integer, intent(in) :: nprocs
      integer(kind = kint), intent(in) :: itype_file, istep_ucd
!
      integer :: ip, id_rank
      character(len=kchara) :: file_name
!
!
      do ip = 1, nprocs
        id_rank = ip - 1
        file_name = set_parallel_ucd_file_name                          &
     &            (file_prefix,  itype_file, id_rank, istep_ucd)
!
        call delete_file_by_f(file_name)
      end do
!
      end subroutine delete_para_ucd_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      character(len=kchara) function set_parallel_ucd_file_name         &
     &                  (file_prefix, itype_file, id_rank, istep_ucd)
!
      use set_parallel_file_name
      use set_mesh_extensions
      use set_ucd_extensions
!
      integer, intent(in) :: id_rank
      integer(kind=kint), intent(in) :: itype_file, istep_ucd
      character(len=kchara), intent(in) ::    file_prefix
      character(len=kchara) :: fname_tmp, file_name
!
!
      fname_tmp = add_int_suffix(istep_ucd, file_prefix)
!
      if (id_rank .ge. 0                                                &
     &      .and. (itype_file/icent) .eq. (iflag_para/icent)) then
        file_name = add_process_id(id_rank, fname_tmp)
      else
        file_name = fname_tmp
      end if
!
      if(mod(itype_file,iten) .eq. iflag_bin) then
        fname_tmp = add_flb_extension(file_name)
        file_name = fname_tmp
        return
      else if(mod(itype_file,icent)/iten .eq. iflag_vtk/iten) then
        fname_tmp = add_vtk_extension(file_name)
      else if(mod(itype_file,icent)/iten .eq. iflag_vtd/iten) then
        fname_tmp = add_vtd_extension(file_name)
      else if(mod(itype_file,icent)/iten .eq. iflag_ucd/iten) then
        fname_tmp = add_ucd_extension(file_name)
      else if(mod(itype_file,icent)/iten .eq. iflag_udt/iten) then
        fname_tmp = add_udt_extension(file_name)
      else
        fname_tmp = add_fld_extension(file_name)
      end if
!
      if (   mod(itype_file,iten) .eq. iflag_gzip) then
        file_name = add_gzip_extension(fname_tmp)
      else
        file_name = fname_tmp
      end if
      set_parallel_ucd_file_name = file_name
!
      end function set_parallel_ucd_file_name
!
!------------------------------------------------------------------
!
      character(len=kchara) function set_parallel_grd_file_name         &
     &                    (file_prefix, itype_file, id_rank)
!
      use set_parallel_file_name
      use set_ucd_extensions
!
      integer, intent(in) :: id_rank
      integer(kind=kint), intent(in) :: itype_file
      character(len=kchara), intent(in) ::    file_prefix
      character(len=kchara) :: fname_tmp, file_name
!
!
      fname_tmp = add_int_suffix(izero, file_prefix)
!
      if (id_rank .ge. 0                                                &
     &     .and. itype_file/icent .eq. iflag_para/icent) then
        file_name = add_process_id(id_rank, fname_tmp)
      else
        file_name = fname_tmp
      end if
!
      if     (mod(itype_file,icent)/iten .eq. iflag_vtd/iten) then
        fname_tmp = add_vtg_extension(file_name)
      else if(mod(itype_file,icent)/iten .eq. iflag_udt/iten) then
        fname_tmp = add_grd_extension(file_name)
      else
        fname_tmp = file_name
      end if
!
      if (   mod(itype_file,iten) .eq. iflag_gzip) then
        file_name = add_gzip_extension(fname_tmp)
      else
        file_name = fname_tmp
      end if
      set_parallel_grd_file_name = file_name
!
      end function set_parallel_grd_file_name
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      character(len=kchara) function set_single_ucd_file_name           &
     &                    (file_prefix, itype_file, istep_ucd)
!
      use set_parallel_file_name
      use set_mesh_extensions
      use set_ucd_extensions
!
      integer(kind=kint), intent(in) :: itype_file, istep_ucd
      character(len=kchara), intent(in) ::    file_prefix
      character(len=kchara) :: fname_tmp, file_name
!
!
      file_name = add_int_suffix(istep_ucd, file_prefix)
!
      if (    mod(itype_file,icent)/iten .eq. iflag_vtk/iten) then
        fname_tmp = add_vtk_extension(file_name)
      else if(mod(itype_file,icent)/iten .eq. iflag_vtd/iten) then
        fname_tmp = add_vtd_extension(file_name)
      else if(mod(itype_file,icent)/iten .eq. iflag_ucd/iten) then
        fname_tmp = add_ucd_extension(file_name)
      else if(mod(itype_file,icent)/iten .eq. iflag_udt/iten) then
        fname_tmp = add_udt_extension(file_name)
      else
        fname_tmp = add_fld_extension(file_name)
      end if
!
      if (   mod(itype_file,iten) .eq. iflag_gzip) then
        file_name = add_gzip_extension(fname_tmp)
      else
        file_name = fname_tmp
      end if
      set_single_ucd_file_name = file_name
!
      end function set_single_ucd_file_name
!
!------------------------------------------------------------------
!
      character(len=kchara) function                                    &
     &          set_single_grd_file_name(file_prefix, itype_file)
!
      use set_parallel_file_name
      use set_ucd_extensions
!
      integer(kind=kint), intent(in) :: itype_file
      character(len=kchara), intent(in) ::    file_prefix
      character(len=kchara) :: fname_tmp, file_name
!
!
      file_name = add_int_suffix(izero, file_prefix)
!
      if     (mod(itype_file,icent)/iten .eq. iflag_vtd/iten) then
        fname_tmp = add_vtg_extension(file_name)
      else if(mod(itype_file,icent)/iten .eq. iflag_udt/iten) then
        fname_tmp = add_grd_extension(file_name)
      else
        fname_tmp = file_name
      end if
!
      if (   mod(itype_file,iten) .eq. iflag_gzip) then
        file_name = add_gzip_extension(fname_tmp)
      else
        file_name = fname_tmp
      end if
      set_single_grd_file_name = file_name
!
      end function set_single_grd_file_name
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      character(len=kchara) function                                    &
     &          set_merged_hdf_mesh_file_name(file_prefix)
!
      use set_ucd_extensions
!
      character(len=kchara), intent(in) ::    file_prefix
      character(len=kchara) :: fname_tmp
!
!
      fname_tmp = add_mesh_suffix(file_prefix)
      set_merged_hdf_mesh_file_name = add_hdf_extension(fname_tmp)
!
      end function set_merged_hdf_mesh_file_name
!
!------------------------------------------------------------------
!
      character(len=kchara) function                                    &
     &         set_merged_hdf_field_file_name(file_prefix, istep_ucd)
!
      use set_parallel_file_name
      use set_ucd_extensions
!
      character(len=kchara), intent(in) ::    file_prefix
      integer(kind=kint), intent(in) :: istep_ucd
      character(len=kchara) :: fname_tmp
!
!
      fname_tmp = add_field_suffix(file_prefix)
      fname_tmp = add_int_suffix(istep_ucd, fname_tmp)
      set_merged_hdf_field_file_name = add_hdf_extension(fname_tmp)
!
      end function set_merged_hdf_field_file_name
!
!------------------------------------------------------------------
!
      character(len=kchara) function                                    &
     &          set_merged_snap_xdmf_file_name(file_prefix, istep_ucd)
!
      use set_parallel_file_name
      use set_ucd_extensions
!
      character(len=kchara), intent(in) ::    file_prefix
      integer(kind=kint), intent(in) :: istep_ucd
!
      character(len=kchara) :: fname_tmp
!
!
      fname_tmp = trim(add_int_suffix(istep_ucd, file_prefix))
      set_merged_snap_xdmf_file_name = add_xdmf_extension(fname_tmp)
!
      end function set_merged_snap_xdmf_file_name
!
!------------------------------------------------------------------
!
      character(len=kchara) function                                    &
     &                     set_merged_xdmf_file_name(file_prefix)
!
      use set_ucd_extensions
!
      character(len=kchara), intent(in) ::file_prefix
      character(len=kchara) :: fname_tmp
!
!
      fname_tmp = add_solution_suffix(file_prefix)
      set_merged_xdmf_file_name = add_xdmf_extension(fname_tmp)
!
      end function set_merged_xdmf_file_name
!
!------------------------------------------------------------------
!
      end module set_ucd_file_names
