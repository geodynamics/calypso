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
!!     &         (file_param, num_pe, istep_fld)
!!        type(field_IO_params), intent(in) :: file_param
!!      subroutine delete_SPH_fld_file                                  &
!!     &         (file_param, num_pe, istep_fld)
!!        type(field_IO_params), intent(in) :: file_param
!!      subroutine delete_SPH_fld_file(itype_file, num_pe, istep_fld)
!!
!!      character(len=kchara) function set_FEM_fld_file_name            &
!!     &                   (file_header, itype_file, id_rank, istep_fld)
!!      character(len=kchara) function set_SPH_fld_file_name            &
!!     &                   (file_header, itype_file, id_rank, istep_fld)
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
     &         (file_param, num_pe, istep_fld)
!
      use t_file_IO_parameter
      use delete_data_files
!
      type(field_IO_params), intent(in) :: file_param
      integer, intent(in) :: num_pe
      integer(kind=kint), intent(in) :: istep_fld
!
      integer :: ip, id_rank
      character(len=kchara) :: file_name
!
!
      do ip =1, num_pe
        id_rank = ip - 1
!
        file_name = set_FEM_fld_file_name                               &
     &          (file_param%file_prefix, file_param%iflag_format,       &
     &           id_rank, istep_fld)
!
        call delete_file_by_f(file_name)
      end do
!
      end subroutine delete_FEM_fld_file
!
!------------------------------------------------------------------
!
      subroutine delete_SPH_fld_file                                    &
     &         (file_param, num_pe, istep_fld)
!
      use t_file_IO_parameter
      use delete_data_files
!
      type(field_IO_params), intent(in) :: file_param
      integer, intent(in) :: num_pe
      integer(kind=kint), intent(in) :: istep_fld
!
      integer :: ip, id_rank
      character(len=kchara) :: file_name
!
!
      do ip =1, num_pe
        id_rank = ip - 1
!
        file_name = set_SPH_fld_file_name                               &
     &          (file_param%file_prefix, file_param%iflag_format,       &
     &           id_rank, istep_fld)
!
        call delete_file_by_f(file_name)
      end do
!
      end subroutine delete_SPH_fld_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      character(len=kchara) function set_FEM_fld_file_name              &
     &                   (file_header, itype_file, id_rank, istep_fld)
!
      use set_parallel_file_name
      use set_mesh_extensions
      use m_file_format_switch
!
      integer, intent(in) :: id_rank
      integer(kind=kint), intent(in) :: itype_file, istep_fld
      character(len=kchara), intent(in) :: file_header
      character(len=kchara) :: fname_tmp, file_name
!
!
      if(istep_fld .eq. iminus) then
        fname_tmp = add_elaps_postfix(file_header)
      else
        fname_tmp = add_int_suffix(istep_fld, file_header)
      end if
!
      if((itype_file/iflag_single) .eq. 0) then
        file_name = add_process_id(id_rank, fname_tmp)
      else
        file_name = fname_tmp
      end if
!
      if     (mod(itype_file,iten) .eq. id_gzip_bin_file_fmt) then
        fname_tmp = add_flb_extension(file_name)
        file_name = add_gzip_extension(fname_tmp)
      else if(mod(itype_file,iten) .eq. id_gzip_txt_file_fmt) then
        fname_tmp = add_fld_extension(file_name)
        file_name = add_gzip_extension(fname_tmp)
      else if(mod(itype_file,iten) .eq. id_binary_file_fmt) then
        fname_tmp = add_flb_extension(file_name)
        file_name = fname_tmp
      else
        fname_tmp = add_fld_extension(file_name)
        file_name = fname_tmp
      end if
      set_FEM_fld_file_name = file_name
!
      end function set_FEM_fld_file_name
!
!------------------------------------------------------------------
!
      character(len=kchara) function set_SPH_fld_file_name              &
     &                   (file_header, itype_file, id_rank, istep_fld)
!
      use set_parallel_file_name
      use set_sph_extensions
      use m_file_format_switch
!
      integer, intent(in) :: id_rank
      integer(kind=kint), intent(in) :: itype_file, istep_fld
      character(len=kchara), intent(in) ::    file_header
      character(len=kchara) :: fname_tmp, file_name
!
!
      if(istep_fld .eq. iminus) then
        fname_tmp = add_elaps_postfix(file_header)
      else
        fname_tmp = add_int_suffix(istep_fld, file_header)
      end if
!
      if((itype_file/iflag_single) .eq. 0) then
        file_name = add_process_id(id_rank, fname_tmp)
      else
        file_name = fname_tmp
      end if
!
      if     (mod(itype_file,iten) .eq. id_gzip_bin_file_fmt) then
        fname_tmp = add_fsb_extension(file_name)
        file_name = add_gzip_extension(fname_tmp)
      else if(mod(itype_file,iten) .eq. id_gzip_txt_file_fmt) then
        fname_tmp = add_fst_extension(file_name)
        file_name = add_gzip_extension(fname_tmp)
      else if(mod(itype_file,iten) .eq. id_binary_file_fmt) then
        fname_tmp = add_fsb_extension(file_name)
        file_name = fname_tmp
      else
        fname_tmp = add_fst_extension(file_name)
        file_name = fname_tmp
      end if
      set_SPH_fld_file_name = file_name
!
      end function set_SPH_fld_file_name
!
!------------------------------------------------------------------
!
      end module set_field_file_names
