!m_geometry_data_MHD.f90
!     module m_geometry_data_MHD
!
!     written by H. Matsui on May, 2009
!
!      subroutine allocate_fluid_node_list
!      subroutine allocate_conduct_node_list
!      subroutine allocate_inner_core_ele_list
!      subroutine allocate_element_connect_org(numele, nnod_4_ele)
!
!      subroutine allocate_geometry_fluid_smp
!      subroutine allocate_geometry_conduct_smp
!      subroutine allocate_geometry_ins_smp
!      subroutine allocate_geometry_ins_smp
!
!      subroutine deallocate_fluid_node_list
!      subroutine deallocate_conduct_node_list
!      subroutine deallocate_inner_core_ele_list
!      subroutine deallocate_element_connect_org
!
      module m_geometry_data_MHD
!
      use m_precision
!
      implicit  none
!
!   for fluid layer
!
      integer( kind=kint )  ::  numnod_fluid
!     number of node on local PE (include external node)
      integer( kind=kint )  ::  internal_node_fluid
!     number of node on local PE
      integer( kind=kint )  ::  iele_fl_start, iele_fl_end
!     start and end element ID for fluid
      integer(kind=kint),  allocatable :: inod_fluid(:)
!
      integer( kind=kint ), allocatable :: iele_fl_smp_stack(:)
!     smp stack of element on local PE
      integer( kind=kint ), allocatable :: inod_fl_smp_stack(:)
!     smp stack of node on local PE
      integer( kind=kint ), allocatable :: inter_fl_smp_stack(:)
!     smp stack of internal node on local PE
!
      integer( kind=kint )  ::  maxnod_fl_smp
!     smp stack of node on local PE
      integer( kind=kint )  ::  max_in_nod_fl_smp
!     smp stack of node on local PE
      integer( kind=kint )  ::  maxele_fl_smp
!     smp stack of node on local PE
!
      real(kind=kreal) :: vol_fluid
!     Fluid Volume
      real(kind=kreal) :: a_vol_fl
!     1 / (Fluid Volume)
!
!
!   for conductive layer
!
      integer( kind=kint )  ::  numnod_conduct
!     number of node on local PE (include external node)
      integer( kind=kint )  ::  internal_node_conduct
!     number of node on local PE
      integer( kind=kint )  ::  iele_cd_start, iele_cd_end
!     start and end element ID for conductor
      integer(kind=kint), allocatable :: inod_conduct(:)
!  
      integer( kind=kint ), allocatable :: iele_cd_smp_stack(:)
!     smp stack of element on local PE
      integer( kind=kint ), allocatable :: inod_cd_smp_stack(:)
!     smp stack of node on local PE
      integer( kind=kint ), allocatable :: inter_cd_smp_stack(:)
!     smp stack of internal node on local PE
!
      integer( kind=kint )  ::  maxnod_cd_smp
!     smp stack of node on local PE
      integer( kind=kint )  ::  max_in_nod_cd_smp
!     smp stack of node on local PE
      integer( kind=kint )  ::  maxele_cd_smp
!     smp stack of node on local PE
!
      real(kind=kreal) :: vol_conduct
!     Conductor Volume
      real(kind=kreal) :: a_vol_cd
!     1 / (Conductor Volume)
!
!   for insulate layer
!
      integer( kind=kint ), allocatable :: iele_ins_smp_stack(:)
!     smp stack of element on local PE
      integer( kind=kint ), allocatable :: inod_ins_smp_stack(:)
!     number of node on local PE
      integer( kind=kint ), allocatable :: inter_ins_smp_stack(:)
!     smp stack of internal node on local PE
!
      integer( kind=kint )  ::  maxnod_ins_smp
!     smp stack of node on local PE
      integer( kind=kint )  ::  max_in_nod_ins_smp
!     smp stack of node on local PE
      integer( kind=kint )  ::  maxele_ins_smp
!     smp stack of node on local PE
!
      integer( kind=kint )  ::  numnod_insulate
!     number of node on local PE (include external node)
      integer( kind=kint )  ::  internal_node_insulate
!     number of node on local PE
      integer( kind=kint )  ::  iele_ins_start, iele_ins_end
!     start and end element ID for insulator
      integer(kind=kint), allocatable :: inod_insulate(:)
!  
        real(kind=kreal) :: vol_insulate
!     Insulator Volume
        real(kind=kreal) :: a_vol_ins
!     1 / (Insulator Volume)
!
!
!   for insulated core
!
      integer( kind=kint ), allocatable :: iele_in_core_smp_stack(:)
!     smp stack of element on local PE
      integer( kind=kint ), allocatable :: inod_in_core_smp_stack(:)
!     smp stack of node on local PE
      integer( kind=kint ), allocatable :: inter_in_core_smp_stack(:)
!     smp stack of internal node on local PE
!
      integer( kind=kint )  ::  maxnod_in_core_smp
!     smp stack of node on local PE
      integer( kind=kint )  ::  max_in_nod_in_core_smp
!     smp stack of node on local PE
      integer( kind=kint )  ::  maxele_in_core_smp
!     smp stack of node on local PE
!
      integer( kind=kint )  ::  numnod_in_core
!     number of node on local PE (include external node)
      integer( kind=kint )  ::  internal_node_in_core
!     number of node on local PE
      integer( kind=kint )  ::  numele_in_core
!     number of element on local PE
!
      integer(kind=kint), allocatable :: inod_in_core(:)
      integer(kind=kint), allocatable :: iele_in_core(:)
      real(kind=kreal) :: vol_i_core
!
!   original connectivity table
!
      integer(kind=kint), allocatable, target  :: ie_org(:,:)
!   element connectivity  (where i:nodal order j:element id)
      integer(kind=kint_gl), allocatable, target :: iele_global_org(:)
!   global element id (where i:element id)
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
       subroutine allocate_fluid_node_list
!
!
       allocate(inod_fluid(numnod_fluid))
       if (numnod_fluid.gt.0) inod_fluid = 0
!
       end subroutine allocate_fluid_node_list
!
! ----------------------------------------------------------------------
!
       subroutine allocate_conduct_node_list
!
!
       allocate(inod_conduct(numnod_conduct))
       allocate(inod_insulate(numnod_insulate))
       allocate(inod_in_core(numnod_in_core))
!
       if(numnod_conduct.gt.0) inod_conduct =   0
       if(numnod_insulate.gt.0) inod_insulate = 0
       if(numnod_in_core.gt.0) inod_in_core =   0
!
       end subroutine allocate_conduct_node_list
!
! ----------------------------------------------------------------------
!
       subroutine allocate_inner_core_ele_list
!
!
       allocate(iele_in_core(numele_in_core))
       if(numele_in_core.gt.0) iele_in_core =   0
!
       end subroutine allocate_inner_core_ele_list
!
! ----------------------------------------------------------------------
!
      subroutine allocate_element_connect_org(numele, nnod_4_ele)
!
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
!
!
      allocate(iele_global_org(numele))
      allocate(ie_org(numele,nnod_4_ele))
      iele_global_org = 0
      ie_org =          0
!
      end subroutine allocate_element_connect_org
!
!------------------------------------------------------------------
!
       subroutine allocate_geometry_fluid_smp
!
       use m_machine_parameter
!
       allocate( iele_fl_smp_stack(0:np_smp))
       allocate( inod_fl_smp_stack(0:np_smp))
       allocate( inter_fl_smp_stack(0:np_smp))
!
       iele_fl_smp_stack = 0
       inod_fl_smp_stack = 0
       inter_fl_smp_stack = 0
!
       end subroutine allocate_geometry_fluid_smp
!
!-----------------------------------------------------------------------
!
       subroutine allocate_geometry_conduct_smp
!
       use m_machine_parameter
!
!   for conductive layer
!
       allocate( iele_cd_smp_stack(0:np_smp))
       allocate( inod_cd_smp_stack(0:np_smp))
       allocate( inter_cd_smp_stack(0:np_smp))
!
       iele_cd_smp_stack = 0
       inod_cd_smp_stack = 0
       inter_cd_smp_stack = 0
!
       end subroutine allocate_geometry_conduct_smp
!
! ----------------------------------------------------------------------
!
       subroutine allocate_geometry_ins_smp
!
       use m_machine_parameter
!
!   for insulate layer
!
       allocate( iele_ins_smp_stack(0:np_smp))
       allocate( inod_ins_smp_stack(0:np_smp))
       allocate( inter_ins_smp_stack(0:np_smp))
!
       iele_ins_smp_stack = 0
       inod_ins_smp_stack = 0
       inter_ins_smp_stack = 0
!
       end subroutine allocate_geometry_ins_smp
!
! ----------------------------------------------------------------------
!
       subroutine allocate_geometry_incore_smp
!
       use m_machine_parameter
!
!   for insulated core
!
       allocate( iele_in_core_smp_stack(0:np_smp))
       allocate( inod_in_core_smp_stack(0:np_smp))
       allocate( inter_in_core_smp_stack(0:np_smp))
!
       iele_in_core_smp_stack = 0
       inod_in_core_smp_stack = 0
       inter_in_core_smp_stack = 0
!
       end subroutine allocate_geometry_incore_smp
!
! ----------------------------------------------------------------------
!------------------------------------------------------------------
!
       subroutine deallocate_fluid_node_list
!
        deallocate(inod_fluid)
!
       end subroutine deallocate_fluid_node_list
!
! ----------------------------------------------------------------------
!
       subroutine deallocate_conduct_node_list
!
!
       deallocate(inod_conduct, inod_insulate)
       deallocate(inod_in_core)
!
       end subroutine deallocate_conduct_node_list
!
! ----------------------------------------------------------------------
!
       subroutine deallocate_inner_core_ele_list
!
!
       deallocate(iele_in_core)
!
       end subroutine deallocate_inner_core_ele_list
!
! ----------------------------------------------------------------------
!
       subroutine deallocate_element_connect_org
!
       deallocate(ie_org, iele_global_org)
!
       end subroutine deallocate_element_connect_org
!
!------------------------------------------------------------------
!
       subroutine deallocate_geometry_fluid_smp
!
       deallocate( iele_fl_smp_stack )
       deallocate( inod_fl_smp_stack )
       deallocate( inter_fl_smp_stack )
!
       end subroutine deallocate_geometry_fluid_smp
!
!-----------------------------------------------------------------------
!
       subroutine deallocate_geometry_conduct_smp
!
       deallocate( iele_cd_smp_stack  )
       deallocate( inod_cd_smp_stack  )
       deallocate( inter_cd_smp_stack )
!
       end subroutine deallocate_geometry_conduct_smp
!
! ----------------------------------------------------------------------
!
       subroutine deallocate_geometry_ins_smp
!
       deallocate( iele_ins_smp_stack  )
       deallocate( inod_ins_smp_stack  )
       deallocate( inter_ins_smp_stack )
!
       end subroutine deallocate_geometry_ins_smp
!
! ----------------------------------------------------------------------
!
       subroutine deallocate_geometry_incore_smp
!
       deallocate( iele_in_core_smp_stack  )
       deallocate( inod_in_core_smp_stack  )
       deallocate( inter_in_core_smp_stack )
!
       end subroutine deallocate_geometry_incore_smp
!
! ----------------------------------------------------------------------
!
      subroutine check_geometry_fluid_smp(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
       write(*,*) 'PE: ', my_rank,                                      &
     &           'inod_fl_smp_stack ', inod_fl_smp_stack
       write(*,*) 'PE: ', my_rank,                                      &
     &           'internal_node_fluid ', internal_node_fluid
       write(*,*) 'PE: ', my_rank,                                      &
     &           'inter_fl_smp_stack ', inter_fl_smp_stack
       write(*,*) 'PE: ', my_rank,                                      &
     &           'iele_fl_smp_stack ', iele_fl_smp_stack
!
      end subroutine check_geometry_fluid_smp
!
! ----------------------------------------------------------------------
!
      subroutine check_geometry_conduct_smp(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
       write(*,*) 'PE: ', my_rank, 'numnod_conduct ', numnod_conduct
       write(*,*) 'PE: ', my_rank,                                      &
     &           'inod_cd_smp_stack ', inod_cd_smp_stack
       write(*,*) 'PE: ', my_rank,                                      &
     &           'internal_node_conduct ', internal_node_conduct
       write(*,*) 'PE: ', my_rank,                                      &
     &           'inter_cd_smp_stack ', inter_cd_smp_stack
       write(*,*) 'PE: ', my_rank,                                      &
     &           'iele_cd_smp_stack ', iele_cd_smp_stack
!
      end subroutine check_geometry_conduct_smp
!
! ----------------------------------------------------------------------
!
      end module m_geometry_data_MHD
