import React from "react";
import styled from "styled-components";

const ImageWrapper = styled.div`
  display: flex;

  > img {
    margin: 0.25em;
    border-radius: 50%;
    max-width: 75px;
    max-height: 75px;
  }
`;

export function Avatar({ show = true, src, alt }) {
  return (
    show && (
      <ImageWrapper>
        <img src={src} alt={alt} />
      </ImageWrapper>
    )
  );
}

export default Avatar;
