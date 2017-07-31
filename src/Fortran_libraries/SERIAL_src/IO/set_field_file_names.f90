!>@file   set_field_file_names.f90
!!@brief  module set_field_file_names
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2007
!
!>@brief  Set file names for field data file
!!
!!@verbatim
!!      subroutine delete_FEM_fld_file                                  &
!!     &         (file_param, nprocs, istep_fld)
!!        type(field_IO_params), intent(in) :: file_param
!!      subroutine delete_SPH_fld_file                                  &
!!     &         (file_param, nprocs, istep_fld)
!!        type(field_IO_params), intent(in) :: file_param
!!      subroutine delete_SPH_fld_file(itype_file, nprocs, istep_fld)
!!
!!      subroutine set_fld_file_name(file_header, itype_file,           &
!!     &          my_rank, istep_fld, file_name)
!!      subroutine set_SPH_fld_file_name(file_header, itype_file,       &
!!     &          my_rank, istep_fld, file_name)
!!@endverbatim
!
      module set_field_file_names
!
      use m_precision
      use m_constants
!
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
      subroutine delete_FEM_fld_file                                    &
     &         (file_param, nprocs, istep_fld)
!
      use t_file_IO_parameter
      use delete_data_files
!
      type(field_IO_params), intent(in) :: file_param
      integer(kind=kint), intent(in) :: nprocs, istep_fld
!
      integer(kind=kint) :: my_rank, ip
      character(len=kchara) :: file_name
!
!
      do ip =1, nprocs
        my_rank = ip - 1
!
        call set_FEM_fld_file_name                                      &
     &     (file_param%file_prefix, file_param%iflag_format,            &
     &      my_rank, istep_fld, file_name)
!
        call delete_file_by_f(file_name)
      end do
!
      end subroutine delete_FEM_fld_file
!
!------------------------------------------------------------------
!
      subroutine delete_SPH_fld_file                                    &
     &         (file_param, nprocs, istep_fld)
!
      use t_file_IO_parameter
      use delete_data_files
!
      type(field_IO_params), intent(in) :: file_param
      integer(kind=kint), intent(in) :: nprocs, istep_fld
!
      integer(kind=kint) :: my_rank, ip
      character(len=kchara) :: file_name
!
!
      do ip =1, nprocs
        my_rank = ip - 1
!
        call set_SPH_fld_file_name                                      &
     &     (file_param%file_prefix, file_param%iflag_format,            &
     &      my_rank, istep_fld, file_name)
!
        call delete_file_by_f(file_name)
      end do
!
      end subroutine delete_SPH_fld_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine set_FEM_fld_file_name(file_header, itype_file,         &
     &          my_rank, istep_fld, file_name)
!
      use set_parallel_file_name
      use m_file_format_switch
!
      integer(kind=kint), intent(in) :: itype_file, my_rank, istep_fld
      character(len=kchara), intent(in) ::    file_header
      character(len=kchara), intent(inout) :: file_name
      character(len=kchara) :: fname_tmp
!
!
      if(istep_fld .eq. iminus) then
        call add_elaps_postfix(file_header, fname_tmp)
      else
        call add_int_suffix(istep_fld, file_header, fname_tmp)
      end if
!
      if((itype_file/iflag_single) .eq. 0) then
        call add_int_suffix(my_rank, fname_tmp, file_name)
      else
        file_name = fname_tmp
      end if
!
      if     (mod(itype_file,iten) .eq. id_gzip_bin_file_fmt) then
        call add_flb_extension(file_name, fname_tmp)
        call add_gzip_extension(fname_tmp, file_name)
      else if(mod(itype_file,iten) .eq. id_gzip_txt_file_fmt) then
        call add_fld_extension(file_name, fname_tmp)
        call add_gzip_extension(fname_tmp, file_name)
      else if(mod(itype_file,iten) .eq. id_binary_file_fmt) then
        call add_flb_extension(file_name, fname_tmp)
        file_name = fname_tmp
      else
        call add_fld_extension(file_name, fname_tmp)
        file_name = fname_tmp
      end if
!
      end subroutine set_FEM_fld_file_name
!
!------------------------------------------------------------------
!
      subroutine set_SPH_fld_file_name(file_header, itype_file,         &
     &          my_rank, istep_fld, file_name)
!
      use set_parallel_file_name
      use m_file_format_switch
!
      integer(kind=kint), intent(in) :: itype_file, my_rank, istep_fld
      character(len=kchara), intent(in) ::    file_header
      character(len=kchara), intent(inout) :: file_name
      character(len=kchara) :: fname_tmp
!
!
      if(istep_fld .eq. iminus) then
        call add_elaps_postfix(file_header, fname_tmp)
      else
        call add_int_suffix(istep_fld, file_header, fname_tmp)
      end if
!
      if((itype_file/iflag_single) .eq. 0) then
        call add_int_suffix(my_rank, fname_tmp, file_name)
      else
        file_name = fname_tmp
      end if
!
      if     (mod(itype_file,iten) .eq. id_gzip_bin_file_fmt) then
        call add_fsb_extension(file_name, fname_tmp)
        call add_gzip_extension(fname_tmp, file_name)
      else if(mod(itype_file,iten) .eq. id_gzip_txt_file_fmt) then
        call add_fst_extension(file_name, fname_tmp)
        call add_gzip_extension(fname_tmp, file_name)
      else if(mod(itype_file,iten) .eq. id_binary_file_fmt) then
        call add_fsb_extension(file_name, fname_tmp)
        file_name = fname_tmp
      else
        call add_fst_extension(file_name, fname_tmp)
        file_name = fname_tmp
      end if
!
      end subroutine set_SPH_fld_file_name
!
!------------------------------------------------------------------
!
      end module set_field_file_names
