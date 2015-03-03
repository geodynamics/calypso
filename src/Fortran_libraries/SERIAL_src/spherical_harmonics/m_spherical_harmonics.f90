!
!     module m_spherical_harmonics
!.......................................................................
!
!      Written by H.Matsui
!      modified by H. Matsui on June, 2007
!
!       subroutine allocate_index_4_sph(nth)
!       subroutine allocate_spherical_harmonics(nth)
!
!       subroutine deallocate_index_4_sph
!       subroutine deallocate_spherical_harmonics
!
!       subroutine set_radial_coefficients
!
      module m_spherical_harmonics
!
      use m_precision
!
      implicit  none
! 
!
      integer ( kind = kint) :: ltr_tri_sph
      integer ( kind = kint) :: jmax_tri_sph
!
      integer ( kind = kint), dimension(:,:), allocatable:: idx
      real   ( kind = kreal), dimension(:,:), allocatable:: g
      real   ( kind = kreal), dimension(:,:), allocatable:: s
!
      real   ( kind = kreal), dimension(:), allocatable:: vp
      real   ( kind = kreal), dimension(:), allocatable:: vt
      real   ( kind = kreal), dimension(:), allocatable:: dvp
!
      real   ( kind = kreal), dimension(:), allocatable:: bp
      real   ( kind = kreal), dimension(:), allocatable:: bt
      real   ( kind = kreal), dimension(:), allocatable:: dbp
      real   ( kind = kreal), dimension(:), allocatable:: mp
!
      real   ( kind = kreal), dimension(3) :: v_pole, b_pole
      real   ( kind = kreal), dimension(3) :: v_cart, b_cart
!
      real ( kind = kreal), dimension(30,2) :: alpha 
      real ( kind = kreal), dimension(30,2) :: beta
!
      real ( kind = kreal), dimension(0:10) :: coef_ap 
      real ( kind = kreal) :: ap6_in, ap6_out 
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_index_4_sph(nth)
!
      integer(kind = kint), intent(in) :: nth
!
      ltr_tri_sph =  nth
      jmax_tri_sph = nth*(nth+2)
!
      allocate ( idx(0:jmax_tri_sph,2) )
      allocate ( g(0:jmax_tri_sph,17) )
!
      idx = 0
      g = 0.0d0
!
      end subroutine allocate_index_4_sph
!
! -----------------------------------------------------------------------
!
       subroutine allocate_spherical_harmonics(nth)
!
         integer(kind = kint), intent(in) :: nth
!
!
        call allocate_index_4_sph(nth)
!
        allocate ( s(0:jmax_tri_sph,0:3) )
!
        allocate ( vp(0:jmax_tri_sph) )
        allocate ( vt(0:jmax_tri_sph) )
        allocate ( dvp(0:jmax_tri_sph) )
!
        allocate ( bp(0:jmax_tri_sph) )
        allocate ( bt(0:jmax_tri_sph) )
        allocate ( dbp(0:jmax_tri_sph) )
        allocate ( mp(0:jmax_tri_sph) )
!
        s = 0.0d0
!
        vp = 0.0d0
        vt = 0.0d0
        dvp = 0.0d0
!
        bp = 0.0d0
        bt = 0.0d0
        dbp = 0.0d0
        mp = 0.0d0
!
       end subroutine allocate_spherical_harmonics
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_index_4_sph
!
        deallocate ( idx, g )
!
       end subroutine deallocate_index_4_sph
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_spherical_harmonics
!
       call deallocate_index_4_sph
!
        deallocate (s)
        deallocate (vp, vt, dvp )
        deallocate (bp, bt, dbp, mp )
!
       end subroutine deallocate_spherical_harmonics
!
! -----------------------------------------------------------------------
!
       subroutine set_radial_coefficients
!
        alpha(1,1)  = 1.49175873625
        alpha(2,1)  = 1.78502609064
        alpha(3,1)  = 1.98703985532
        alpha(4,1)  = 2.13664596174
        alpha(5,1)  = 2.25236214226
        alpha(6,1)  = 2.34462213562
        alpha(7,1)  = 2.4198939892
        alpha(8,1)  = 2.48244544414
        alpha(9,1)  = 2.53522096477
        alpha(10,1) = 2.58032248883
        alpha(11,1) = 2.61929202477
        alpha(12,1) = 2.65328667355
        alpha(13,1) = 2.68319153225
        alpha(14,1) = 2.70969496513
        alpha(15,1) = 2.73334018041
        alpha(16,1) = 2.75456140608
        alpha(17,1) = 2.77370978253
        alpha(18,1) = 2.7910722279
        alpha(19,1) = 2.80688540311
        alpha(20,1) = 2.8213461975
        alpha(21,1) = 2.83461970411
        alpha(22,1) = 2.84684535712
        alpha(23,1) = 2.85814170602
        alpha(24,1) = 2.86861016617
        alpha(25,1) = 2.87833799214
        alpha(26,1) = 2.88740065508
        alpha(27,1) = 2.89576375834
        alpha(28,1) = 2.90378459259
        alpha(29,1) = 2.91121340677
        alpha(30,1) = 2.91819445364
!
        beta( 1,1)  = 2.10385462627
        beta( 2,1)  = 2.3831791958
        beta( 3,1)  = 2.57568428909
        beta( 4,1)  = 2.71769313136
        beta( 5,1)  = 2.82714541902
        beta( 6,1)  = 2.91418450828
        beta( 7,1)  = 2.98506986616
        beta( 8,1)  = 3.04390665391
        beta( 9,1)  = 3.09351122965
        beta(10,1)  = 3.13588457293
        beta(11,1)  = 3.17248903188
        beta(12,1)  = 3.20441862609
        beta(13,1)  = 3.23250822986
        beta(14,1)  = 3.2574059619
        beta(15,1)  = 3.27962255167
        beta(16,1)  = 3.29956582508
        beta(17,1)  = 3.31756530398
        beta(18,1)  = 3.33389007865
        beta(19,1)  = 3.34876200692
        beta(20,1)  = 3.36236560549
        beta(21,1)  = 3.37485556102
        beta(22,1)  = 3.38636250271
        beta(23,1)  = 3.39699748776
        beta(24,1)  = 3.406855522
        beta(25,1)  = 3.41601834874
        beta(26,1)  = 3.42455667754
        beta(27,1)  = 3.43235933063
        beta(28,1)  = 3.43999794142
        beta(29,1)  = 3.44700166077
        beta(30,1)  = 3.45358461951
!
!
        alpha( 1,2) = 1.56546799253
        alpha( 2,2) = 1.8590016146
        alpha( 3,2) = 2.05872722849
        alpha( 4,2) = 2.20505484248
        alpha( 5,2) = 2.31717951674
        alpha( 6,2) = 2.40585430934
        alpha( 7,2) = 2.47769694105
        alpha( 8,2) = 2.53703932927
        alpha( 9,2) = 2.58684628877
        alpha(10,2) = 2.62921791445
        alpha(11,2) = 2.66568365089
        alpha(12,2) = 2.69738343419
        alpha(13,2) = 2.7251838344
        alpha(14,2) = 2.74975499513
        alpha(15,2) = 2.77162301926
        alpha(16,2) = 2.79120648515
        alpha(17,2) = 2.80884242792
        alpha(18,2) = 2.82480514633
        alpha(19,2) = 2.83932014797
        alpha(20,2) = 2.85257433555
        alpha(21,2) = 2.8647240162
        alpha(22,2) = 2.87590082397
        alpha(23,2) = 2.88621639085
        alpha(24,2) = 2.89576598911
        alpha(25,2) = 2.90463140888
        alpha(26,2) = 2.91288325129
        alpha(27,2) = 2.9205827706
        alpha(28,2) = 2.92778336539
        alpha(29,2) = 2.93453179398
        alpha(30,2) = 2.94086917178
!
        beta( 1,2)  = 2.01486189284
        beta( 2,2)  = 2.24974641104
        beta( 3,2)  = 2.40899047872
        beta( 4,2)  = 2.5249845664
        beta( 5,2)  = 2.6134784762
        beta( 6,2)  = 2.68326103263
        beta( 7,2)  = 2.7396935015
        beta( 8,2)  = 2.78625562208
        beta( 9,2)  = 2.82531234658
        beta(10,2)  = 2.85852972647
        beta(11,2)  = 2.88711604852
        beta(12,2)  = 2.91196904845
        beta(13,2)  = 2.93376956991
        beta(14,2)  = 2.95304320772
        beta(15,2)  = 2.97020205493
        beta(16,2)  = 2.98557367689
        beta(17,2)  = 2.99942165339
        beta(18,2)  = 3.01196039198
        beta(19,2)  = 3.02336616763
        beta(20,2)  = 3.03378497882
        beta(21,2)  = 3.04333896814
        beta(22,2)  = 3.05213098876
        beta(23,2)  = 3.06024827282
        beta(24,2)  = 3.06776527849
        beta(25,2)  = 3.07474593816
        beta(26,2)  = 3.08124544914
        beta(27,2)  = 3.08731171165
        beta(28,2)  = 3.09298649266
        beta(29,2)  = 3.09830637431
        beta(30,2)  = 3.10330353209
!
        coef_ap(0)  = 0.12328380762
        coef_ap(1)  = -0.85017260533
        coef_ap(2)  = 2.8033255688
        coef_ap(3)  = -6.0376606546
        coef_ap(4)  = 10.693175584
        coef_ap(5)  = -10.894459506
        coef_ap(6)  = 6.1996192744
        coef_ap(7)  = -2.2832348824
        coef_ap(8)  = 0.69435442851
        coef_ap(9)  = -0.16755555489
        coef_ap(10)  = 0.019676811862
!
        ap6_in = 0.066918 * (13.0d0 / 7.0d0)**3
        ap6_out = 0.26436217043 * (20.d0 / 13.0d0 )**2
!
       end subroutine set_radial_coefficients
!
! -----------------------------------------------------------------------
!
      end module m_spherical_harmonics

