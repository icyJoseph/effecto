import React from "react";
import { FontAwesomeIcon } from "@fortawesome/react-fontawesome";
import { faLinkedin } from "@fortawesome/free-brands-svg-icons";
import { LinkButton } from "../../styles/Buttons";
import { authURL } from "../../endpoints";

export function LinkedIn() {
  return (
    <LinkButton href={authURL}>
      <FontAwesomeIcon icon={faLinkedin} />
    </LinkButton>
  );
}

export default LinkedIn;
