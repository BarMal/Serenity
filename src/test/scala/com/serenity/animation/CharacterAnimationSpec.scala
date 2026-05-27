//package com.serenity.animation
//
//import org.scalatest.funspec.AnyFunSpec
//import org.scalatest.matchers.should.Matchers
//import scala.concurrent.duration.*
//
//class CharacterAnimationSpec extends AnyFunSpec with Matchers:
//
//  describe("AnimationConfig") {
//    describe("opacityForStep") {
//      it("should return 1.0 for single step") {
//        val config = AnimationConfig(1, 100.millis, AnimationType.FadeIn)
//        config.opacityForStep(0) should be(1.0)
//      }
//      
//      it("should return progressive opacity for multiple steps") {
//        val config = AnimationConfig(4, 100.millis, AnimationType.FadeIn)
//        config.opacityForStep(0) should be(0.25)
//        config.opacityForStep(1) should be(0.5)
//        config.opacityForStep(2) should be(0.75)
//        config.opacityForStep(3) should be(1.0)
//      }
//      
//      it("should cap opacity at 1.0 for steps beyond range") {
//        val config = AnimationConfig(3, 100.millis, AnimationType.FadeIn)
//        config.opacityForStep(5) should be(1.0)
//      }
//    }
//    
//    describe("stepDurationMs") {
//      it("should calculate step duration correctly") {
//        val config = AnimationConfig(4, 200.millis, AnimationType.FadeIn)
//        config.stepDurationMs should be(50)
//      }
//      
//      it("should return 0 for zero steps") {
//        val config = AnimationConfig(0, 100.millis, AnimationType.FadeIn)
//        config.stepDurationMs should be(0)
//      }
//    }
//  }
//
//  describe("AnimatingCharacter") {
//    describe("currentOpacity") {
//      it("should return 0.25 at first step") {
//        val config = AnimationConfig(4, 200.millis, AnimationType.FadeIn)
//        val character = AnimatingCharacter('a', 10, 5, 1000L, config)
//        
//        // At 25ms (middle of first step), should be at first opacity level
//        character.currentOpacity(1025L) should be(0.25)
//      }
//      
//      it("should return 0.5 at second step") {
//        val config = AnimationConfig(4, 200.millis, AnimationType.FadeIn)
//        val character = AnimatingCharacter('a', 10, 5, 1000L, config)
//        
//        // At 75ms (middle of second step)
//        character.currentOpacity(1075L) should be(0.5)
//      }
//      
//      it("should return 1.0 when animation is complete") {
//        val config = AnimationConfig(4, 200.millis, AnimationType.FadeIn)
//        val character = AnimatingCharacter('a', 10, 5, 1000L, config)
//        
//        // At 300ms (beyond total duration)
//        character.currentOpacity(1300L) should be(1.0)
//      }
//    }
//    
//    describe("isComplete") {
//      it("should return false during animation") {
//        val config = AnimationConfig(4, 200.millis, AnimationType.FadeIn)
//        val character = AnimatingCharacter('a', 10, 5, 1000L, config)
//        
//        character.isComplete(1100L) should be(false)
//      }
//      
//      it("should return true when animation duration has elapsed") {
//        val config = AnimationConfig(4, 200.millis, AnimationType.FadeIn)
//        val character = AnimatingCharacter('a', 10, 5, 1000L, config)
//        
//        character.isComplete(1200L) should be(true)
//      }
//    }
//  }
//
//  describe("AnimationState") {
//    describe("addAnimation") {
//      it("should add new animation to empty state") {
//        val state = AnimationState.empty
//        val config = AnimationConfig(3, 150.millis, AnimationType.FadeIn)
//        
//        val newState = state.addAnimation('x', 5, 10, config, 2000L)
//        
//        newState.activeAnimations should have size 1
//        val animation = newState.getAnimation(5, 10)
//        animation should be(defined)
//        animation.get.char should be('x')
//      }
//      
//      it("should replace animation at same position") {
//        val state = AnimationState.empty
//        val config = AnimationConfig(3, 150.millis, AnimationType.FadeIn)
//        
//        val state1 = state.addAnimation('x', 5, 10, config, 2000L)
//        val state2 = state1.addAnimation('y', 5, 10, config, 2050L)
//        
//        state2.activeAnimations should have size 1
//        val animation = state2.getAnimation(5, 10)
//        animation.get.char should be('y')
//        animation.get.startTimeMs should be(2050L)
//      }
//      
//      it("should maintain multiple animations at different positions") {
//        val state = AnimationState.empty
//        val config = AnimationConfig(3, 150.millis, AnimationType.FadeIn)
//        
//        val finalState = state
//          .addAnimation('a', 1, 1, config, 1000L)
//          .addAnimation('b', 2, 1, config, 1020L)
//          .addAnimation('c', 1, 2, config, 1040L)
//        
//        finalState.activeAnimations should have size 3
//        finalState.getAnimation(1, 1).get.char should be('a')
//        finalState.getAnimation(2, 1).get.char should be('b')
//        finalState.getAnimation(1, 2).get.char should be('c')
//      }
//    }
//    
//    describe("cleanupCompleted") {
//      it("should remove completed animations") {
//        val state = AnimationState.empty
//        val config = AnimationConfig(3, 100.millis, AnimationType.FadeIn)
//        
//        val stateWithAnimations = state
//          .addAnimation('a', 1, 1, config, 1000L) // Completes at 1100L
//          .addAnimation('b', 2, 1, config, 1050L) // Completes at 1150L
//        
//        // At 1125L, first animation is complete but second is not
//        val cleanedState = stateWithAnimations.cleanupCompleted(1125L)
//        
//        cleanedState.activeAnimations should have size 1
//        cleanedState.getAnimation(1, 1) should be(empty)
//        cleanedState.getAnimation(2, 1) should be(defined)
//      }
//      
//      it("should keep all animations if none are complete") {
//        val state = AnimationState.empty
//        val config = AnimationConfig(3, 100.millis, AnimationType.FadeIn)
//        
//        val stateWithAnimations = state
//          .addAnimation('a', 1, 1, config, 1000L)
//          .addAnimation('b', 2, 1, config, 1050L)
//        
//        val cleanedState = stateWithAnimations.cleanupCompleted(1075L)
//        
//        cleanedState.activeAnimations should have size 2
//      }
//    }
//    
//    describe("hasActiveAnimations") {
//      it("should return false for empty state") {
//        AnimationState.empty.hasActiveAnimations should be(false)
//      }
//      
//      it("should return true when animations exist") {
//        val state = AnimationState.empty
//        val config = AnimationConfig(3, 150.millis, AnimationType.FadeIn)
//        
//        val stateWithAnimation = state.addAnimation('x', 5, 10, config, 1000L)
//        stateWithAnimation.hasActiveAnimations should be(true)
//      }
//    }
//  }
//
//  describe("Predefined AnimationConfigs") {
//    it("should have reasonable default values") {
//      AnimationConfig.quick.get.opacitySteps should be(3)
//      AnimationConfig.quick.get.totalDuration should be(150.millis)
//      
//      AnimationConfig.smooth.get.opacitySteps should be(5)
//      AnimationConfig.smooth.get.totalDuration should be(200.millis)
//      
//      AnimationConfig.subtle.get.opacitySteps should be(2)
//      AnimationConfig.subtle.get.totalDuration should be(100.millis)
//    }
//  }