!>@file   set_ucd_extensions.f90
!!@brief  module set_ucd_extensions
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in 2004
!
!>@brief  Set file extension
!!
!!@verbatim
!!      character(len = kchara) function add_ucd_extension(file_head)
!!                put ".inp" at the end
!!      character(len = kchara) function add_udt_extension(file_head)
!!                put ".udt" at the end
!!      character(len = kchara) function add_grd_extension(file_head)
!!                put ".grd" at the end
!!
!!      character(len = kchara) function add_sdt_extension(file_head)
!!                put ".sdt" at the end
!!      character(len = kchara) function add_sfm_extension(file_head)
!!                put ".sfm" at the end
!!      character(len = kchara) function add_sgd_extension(file_head)
!!                put ".sgd" at the end
!!
!!      character(len = kchara) function add_pvtk_extension(file_head)
!!                put ".pvtk" at the end
!!      character(len = kchara) function add_vtk_extension(file_head)
!!                put ".vtk" at the end
!!      character(len = kchara) function add_vtd_extension(file_head)
!!                put ".vtd" at the end
!!      character(len = kchara) function add_vtg_extension(file_head)
!!                put ".vtg" at the end
!!
!!      character(len = kchara) function add_xdmf_extension(file_head)
!!                put ".xdmf" at the end
!!      character(len = kchara) function add_hdf_extension(file_head)
!!                put ".h5" at the end
!!      character(len = kchara) function add_mesh_suffix(file_head)
!!                put ".mesh" at the end
!!      character(len = kchara) function add_field_suffix(file_head)
!!                put ".field" at the end
!!      character(len = kchara) function add_solution_suffix(file_head)
!!                put ".solution" at the end
!!      character(len = kchara) function add_node_extension(file_head)
!!                put ".node.dat" at the end
!!      character(len = kchara) function add_connect_extension(file_head)
!!                put ".connect.dat" at the end
!!
!!      character(len = kchara) function add_dx_extension(file_head)
!!                put ".dx" at the end
!!@endverbatim
!!
!!@n @param file_head      file prefix
!!
      module set_ucd_extensions
!
      use m_precision
      use set_parallel_file_name
!
      implicit  none
!
      character(len=3), parameter :: inp_ext = "inp"
      character(len=3), parameter :: udt_ext = "udt"
      character(len=3), parameter :: grd_ext = "grd"
!
      character(len=3), parameter :: sfm_ext = "sfm"
      character(len=3), parameter :: sdt_ext = "sdt"
      character(len=3), parameter :: sgd_ext = "sgd"
!
      character(len=3), parameter :: vtk_ext = "vtk"
      character(len=3), parameter :: vtd_ext = "vtd"
      character(len=3), parameter :: vtg_ext = "vtg"
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      character(len = kchara) function add_ucd_extension(file_head)
!
      character(len=kchara), intent(in) :: file_head
!
      add_ucd_extension = add_3chara_extension(file_head, inp_ext)
!
      end function add_ucd_extension
!
!-----------------------------------------------------------------------
!
      character(len = kchara) function add_udt_extension(file_head)
!
      character(len=kchara), intent(in) :: file_head
!
      add_udt_extension = add_3chara_extension(file_head, udt_ext)
!
      end function add_udt_extension
!
!-----------------------------------------------------------------------
!
      character(len = kchara) function add_grd_extension(file_head)
!
      character(len=kchara), intent(in) :: file_head
!
      add_grd_extension = add_3chara_extension(file_head, grd_ext)
!
      end function add_grd_extension
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      character(len = kchara) function add_sdt_extension(file_head)
!
      character(len=kchara), intent(in) :: file_head
!
      add_sdt_extension = add_3chara_extension(file_head, sdt_ext)
!
      end function add_sdt_extension
!
!-----------------------------------------------------------------------
!
      character(len = kchara) function add_sfm_extension(file_head)
!
      character(len=kchara), intent(in) :: file_head
!
      add_sfm_extension = add_3chara_extension(file_head, sfm_ext)
!
      end function add_sfm_extension
!
!-----------------------------------------------------------------------
!
      character(len = kchara) function add_sgd_extension(file_head)
!
      character(len=kchara), intent(in) :: file_head
!
      add_sgd_extension = add_3chara_extension(file_head, sgd_ext)
!
      end function add_sgd_extension
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      character(len = kchara) function add_pvtk_extension(file_head)
!
      character(len=kchara), intent(in) :: file_head
!
       write(add_pvtk_extension,1011) trim(file_head)
 1011 format (a,".pvtk")
!
      end function add_pvtk_extension
!
!-----------------------------------------------------------------------
!
      character(len = kchara) function add_vtk_extension(file_head)
!
      character(len=kchara), intent(in) :: file_head
!
      add_vtk_extension = add_3chara_extension(file_head, vtk_ext)
!
      end function add_vtk_extension
!
!-----------------------------------------------------------------------
!
      character(len = kchara) function add_vtd_extension(file_head)
!
      character(len=kchara), intent(in) :: file_head
!
      add_vtd_extension = add_3chara_extension(file_head, vtd_ext)
!
      end function add_vtd_extension
!
!-----------------------------------------------------------------------
!
      character(len = kchara) function add_vtg_extension(file_head)
!
      character(len=kchara), intent(in) :: file_head
!
      add_vtg_extension = add_3chara_extension(file_head, vtg_ext)
!
      end function add_vtg_extension
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      character(len = kchara) function add_xdmf_extension(file_head)
!
      character(len=kchara), intent(in) :: file_head
!
      write(add_xdmf_extension,1011) trim(file_head)
1011 format (a,".xdmf")
!
      end function add_xdmf_extension
!
!-----------------------------------------------------------------------
!
      character(len = kchara) function add_hdf_extension(file_head)
!
      character(len=kchara), intent(in) :: file_head
!
       write(add_hdf_extension,1011) trim(file_head)
 1011 format (a,".h5")
!
      end function add_hdf_extension
!
!-----------------------------------------------------------------------
!
      character(len = kchara) function add_mesh_suffix(file_head)
!
      character(len=kchara), intent(in) :: file_head
!
      write(add_mesh_suffix,1011) trim(file_head)
1011 format (a,".mesh")
!
      end function add_mesh_suffix
!
!-----------------------------------------------------------------------
!
      character(len = kchara) function add_field_suffix(file_head)
!
      character(len=kchara), intent(in) :: file_head
!
      write(add_field_suffix,1011) trim(file_head)
1011 format (a,".field")
!
      end function add_field_suffix
!
!-----------------------------------------------------------------------
!
      character(len = kchara) function add_solution_suffix(file_head)
!
      character(len=kchara), intent(in) :: file_head
!
      write(add_solution_suffix,1011) trim(file_head)
1011 format (a,".solution")
!
      end function add_solution_suffix
!
!-----------------------------------------------------------------------
!
      character(len = kchara) function add_node_extension(file_head)
!
      character(len=kchara), intent(in) :: file_head
!
       write(add_node_extension,1011) trim(file_head)
 1011 format (a,".node.dat")
!
      end function add_node_extension
!
!-----------------------------------------------------------------------
!
      character(len = kchara) function add_connect_extension(file_head)
!
      character(len=kchara), intent(in) :: file_head
!
       write(add_connect_extension,1011) trim(file_head)
 1011 format (a,".connect.dat")
!
      end function add_connect_extension
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      character(len = kchara) function add_dx_extension(file_head)
!
      character(len=kchara), intent(in) :: file_head
!
       write(add_dx_extension,1011) trim(file_head)
 1011 format (a,".dx")
!
      end function add_dx_extension
!
!-----------------------------------------------------------------------
!
      end module set_ucd_extensions
