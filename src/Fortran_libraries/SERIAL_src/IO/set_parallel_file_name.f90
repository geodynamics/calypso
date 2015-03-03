!>@file   set_parallel_file_name.f90
!!@brief  module set_parallel_file_name
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in 2004
!
!>@brief  Set file extension
!!
!!@verbatim
!!      subroutine delete_directory_name(dir_file_name, file_name)
!!
!!      subroutine add_int_suffix(int_id, file_header, file_name)
!!
!!      subroutine add_dat_extension(file_header, file_name)
!!                put ".dat" at the end
!!      subroutine add_udt_extension(file_header, file_name)
!!                put ".udt" at the end
!!
!!      subroutine add_vtk_extension(file_header, file_name)
!!                put ".vtk" at the end
!!      subroutine add_pvtk_extension(file_header, file_name)
!!                put ".pvtk" at the end
!!      subroutine add_vtd_extension(file_header, file_name)
!!                put ".vtd" at the end
!!      subroutine add_vtg_extension(file_header, file_name)
!!                put ".vtg" at the end
!!
!!      subroutine add_hdf_extension(file_header, file_name)
!!                put ".h5" at the end
!!      subroutine add_xdmf_extension(file_header, file_name)
!!                put ".xdmf" at the end
!!      subroutine add_mesh_suffix(file_header, file_name)
!!                put ".mesh" at the end
!!      subroutine add_field_suffix(file_header, file_name)
!!                put ".field" at the end
!!      subroutine add_xdmf_suffix(file_header, file_name)
!!                put ".solution" at the end
!!
!!      subroutine add_grd_extension(file_header, file_name)
!!                put ".grd" at the end
!!      subroutine add_ucd_extension(file_header, file_name)
!!                put ".inp" at the end
!!
!!      subroutine add_dx_extension(file_header, file_name)
!!                put ".dx" at the end
!!      subroutine add_node_extension(file_header, file_name)
!!                put ".node.dat" at the end
!!      subroutine add_connect_extension(file_header, file_name)
!!                put ".connect.dat" at the end
!!
!!      subroutine add_fld_extension(file_header, file_name)
!!                put ".fld" at the end
!!      subroutine add_flb_extension(file_header, file_name)
!!                put ".flb" at the end
!!      subroutine add_fst_extension(file_header, file_name)
!!                put ".fst" at the end
!!      subroutine add_elaps_postfix(file_header, file_name)
!!                put ".elps" at the end
!!
!!      subroutine add_gzip_extension(file_header, file_name)
!!                put ".gz" and null character at the end
!!
!!      subroutine add_ksm_extension(file_header, file_name)
!!                put ".ksm" at the end
!!
!!      subroutine add_gfm_extension(file_header, file_name)
!!                put ".gfm" at the end
!!
!!      subroutine add_rtp_extension(file_header, file_name)
!!                put ".rtp" at the end
!!      subroutine add_rtm_extension(file_header, file_name)
!!                put ".rtm" at the end
!!      subroutine add_rlm_extension(file_header, file_name)
!!                put ".rlm" at the end
!!      subroutine add_rj_extension(file_header, file_name)
!!                put ".rj" at the end
!!
!!      subroutine add_index_after_name(int_id, chara_head, chara_name)
!!      subroutine int_to_str(int_val, int_string)
!!      subroutine lint_to_str(lint_val, int_string)
!!      subroutine real_to_str(real_val, real_string)
!!@endverbatim
!!
!!@n @param dir_file_name    file name (header) including directory name
!!@n @param file_header      file prefix
!!@n @param int_id           integer to be added at the end of prefix
!!@n @param file_name        output file name
!!@n @param int_val          integer to be tranfered to character
!!@n @param int_string       output character
!

      module set_parallel_file_name
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
      subroutine delete_directory_name(dir_file_name, file_name)
!
      character(len=kchara), intent(in) :: dir_file_name
      character(len=kchara), intent(inout) :: file_name
!
      integer(kind = kint) :: i, len, len_name, idir_flag
      character(len=kchara) :: fmt_txt
!
!
      len_name = len_trim(dir_file_name)
      idir_flag = 0
      do i = len_name, 1, -1
        if(dir_file_name(i:i) .eq. '/') then
          idir_flag = i
          exit
        end if
      end do
      len = len_name - idir_flag
!
      write(fmt_txt,'(a2,i4,a1)') '(a',len,')'
      write(file_name,fmt_txt) dir_file_name(idir_flag+1:len_name)
!
      end subroutine delete_directory_name
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_int_suffix(int_id, file_header, file_name)
!
      integer(kind = kint), intent(in) :: int_id
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
      character(len=kchara) :: chara_head
!
!
      write(chara_head,1000) trim(file_header)
      call add_index_after_name(int_id, chara_head, file_name)
!
 1000 format (a,".")
!
      end subroutine add_int_suffix
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_dat_extension(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
 1011 format (a,".dat")
!
      end subroutine add_dat_extension
!
!-----------------------------------------------------------------------
!
      subroutine add_udt_extension(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
 1011 format (a,".udt")
!
      end subroutine add_udt_extension
!
!-----------------------------------------------------------------------
!
      subroutine add_vtk_extension(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
 1011 format (a,".vtk")
!
      end subroutine add_vtk_extension
!
!-----------------------------------------------------------------------
!
      subroutine add_pvtk_extension(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
 1011 format (a,".pvtk")
!
      end subroutine add_pvtk_extension
!
!-----------------------------------------------------------------------
!
      subroutine add_vtd_extension(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
 1011 format (a,".vtd")
!
      end subroutine add_vtd_extension
!
!-----------------------------------------------------------------------
!
      subroutine add_vtg_extension(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
 1011 format (a,".vtg")
!
      end subroutine add_vtg_extension
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_hdf_extension(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
 1011 format (a,".h5")
!
      end subroutine add_hdf_extension
!
!-----------------------------------------------------------------------
!
      subroutine add_xdmf_extension(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
      write(file_name,1011) trim(file_header)
1011 format (a,".xdmf")
!
      end subroutine add_xdmf_extension
!
!-----------------------------------------------------------------------
!
      subroutine add_mesh_suffix(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
      write(file_name,1011) trim(file_header)
1011 format (a,".mesh")
!
      end subroutine add_mesh_suffix
!
!-----------------------------------------------------------------------
!
      subroutine add_field_suffix(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
      write(file_name,1011) trim(file_header)
1011 format (a,".field")
!
      end subroutine add_field_suffix
!
!-----------------------------------------------------------------------
!
      subroutine add_xdmf_suffix(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
      write(file_name,1011) trim(file_header)
1011 format (a,".solution")
!
      end subroutine add_xdmf_suffix
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_grd_extension(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
      write(file_name,1011) trim(file_header)
 1011 format (a,".grd")
!
      end subroutine add_grd_extension
!
!-----------------------------------------------------------------------
!
      subroutine add_ucd_extension(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
       file_name = trim(file_name)
 1011 format (a,".inp")
!
      end subroutine add_ucd_extension
!
!-----------------------------------------------------------------------
!
      subroutine add_dx_extension(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
 1011 format (a,".dx")
!
      end subroutine add_dx_extension
!
!-----------------------------------------------------------------------
!
      subroutine add_node_extension(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
 1011 format (a,".node.dat")
!
      end subroutine add_node_extension
!
!-----------------------------------------------------------------------
!
      subroutine add_connect_extension(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
 1011 format (a,".connect.dat")
!
      end subroutine add_connect_extension
!
!-----------------------------------------------------------------------
!
      subroutine add_fld_extension(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
 1011 format (a,".fld")
!
      end subroutine add_fld_extension
!
!-----------------------------------------------------------------------
!
      subroutine add_flb_extension(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
 1011 format (a,".flb")
!
      end subroutine add_flb_extension
!
!-----------------------------------------------------------------------
!
      subroutine add_fst_extension(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
 1011 format (a,".fst")
!
      end subroutine add_fst_extension
!
!-----------------------------------------------------------------------
!
      subroutine add_elaps_postfix(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
 1011 format (a,".elps")
!
      end subroutine add_elaps_postfix
!
!-----------------------------------------------------------------------
!
      subroutine add_gzip_extension(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header), ".gz", char(0)
 1011 format (a,a3,a1)
!
      end subroutine add_gzip_extension
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_ksm_extension(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
       file_name = trim(file_name)
 1011 format (a,".ksm")
!
      end subroutine add_ksm_extension
!
!-----------------------------------------------------------------------
!
      subroutine add_gfm_extension(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
       file_name = trim(file_name)
 1011 format (a,".gfm")
!
      end subroutine add_gfm_extension
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_rtp_extension(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
       file_name = trim(file_name)
 1011 format (a,".rtp")
!
      end subroutine add_rtp_extension
!
!-----------------------------------------------------------------------
!
      subroutine add_rtm_extension(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
       file_name = trim(file_name)
 1011 format (a,".rtm")
!
      end subroutine add_rtm_extension
!
!-----------------------------------------------------------------------
!
      subroutine add_rlm_extension(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
       file_name = trim(file_name)
 1011 format (a,".rlm")
!
      end subroutine add_rlm_extension
!
!-----------------------------------------------------------------------
!
      subroutine add_rj_extension(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
       file_name = trim(file_name)
 1011 format (a,".rj")
!
      end subroutine add_rj_extension
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_index_after_name(int_id, chara_head, chara_name)
!
      integer(kind = kint), intent(in) :: int_id
      character(len=kchara), intent(in) :: chara_head
      character(len=kchara), intent(inout) :: chara_name
!
      character(len=kchara) :: charaint
!
!
      write(charaint,*) int_id
      write(chara_name,'(a,a)')                                         &
                trim(chara_head), trim(ADJUSTL(charaint))
!
      end subroutine add_index_after_name
!
!-----------------------------------------------------------------------
!
      subroutine int_to_str(int_val, int_string)
!
      integer(kind=kint), intent(in) :: int_val
      character(len=kchara), intent(inout) :: int_string
      character(len=kchara) :: tmp_string
!
!
      write(tmp_string,*) int_val
      write(int_string,'(a)') trim(adjustl(tmp_string))
!
      end subroutine int_to_str
!
!-----------------------------------------------------------------------
!
      subroutine lint_to_str(lint_val, int_string)
!
      integer(kind=kint_gl), intent(in) :: lint_val
      character(len=kchara), intent(inout) :: int_string
      character(len=kchara) :: tmp_string
!
!
      write(tmp_string,*) lint_val
      write(int_string,'(a)') trim(adjustl(tmp_string))
!
      end subroutine lint_to_str
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine real_to_str(real_val, real_string)
!
      real(kind=kreal), intent(in) :: real_val
      character(len=kchara), intent(inout) :: real_string
      character(len=kchara) :: tmp_string
!
!
      write(tmp_string,*) real_val
      write(real_string,'(a)') trim(adjustl(tmp_string))
!
      end subroutine real_to_str
!
!-----------------------------------------------------------------------
!
      end module set_parallel_file_name
